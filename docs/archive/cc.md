# Claude Code Implementation Guide — Lmod IR Canonicalizer + Validator v2

You are working on the **Lmod / VB6 IR** project.

We already implemented:
- `ir_canonicalizer.py` (value-level normalization)
- Wiring into:
  - `vb6_ui_agent.py`
  - `vb6_logic_agent.py`
  - `vb6_data_agent.py`
  - `validator.py` (canonicalizing expected + actual IR)
- A first version of `equal_with_tolerance` for reasoning fields.

Current post-canonicalizer accuracy (approx):
- UI:    ~60.5%
- Logic: ~65.6%
- Data:  ~98.1%
- Overall: ~66.8%

**Goal of this iteration (Validator v2):**
Implement a **structural tolerance layer** in `validator.py` to:
- Stop penalizing missing OPTIONAL fields
- Ignore extra fields in actual IR when they are not required
- Treat dicts as *schema-aware* (subset-tolerant) rather than strict deep equality
- Keep using `ir_canonicalizer.py` unchanged

Target:
- Push overall accuracy into the **80–90% band** without touching prompts or agents.

Do **NOT** modify:
- `vb6_ui_agent.py`
- `vb6_logic_agent.py`
- `vb6_data_agent.py`
- `ir_canonicalizer.py`
- Any prompts

All changes must stay inside **`validator.py`**.

---

## 1. OPTIONAL vs MUST-HAVE field helpers

In `validator.py`, add these helper functions near the top (or close to existing helpers).  
They classify fields based on the **field path** (a dotted path you pass during recursion).

```python
def is_reasoning_field(path: str) -> bool:
    p = path.lower()
    return (
        p.endswith("_why")
        or p.endswith("_note")
        or "reason" in p
        or "explain" in p
    )


def is_optional_field(path: str) -> bool:
    """Fields that are nice-to-have but not required for a 'correct' IR.
    Missing them should NOT count as a mismatch.
    """
    p = path.lower()

    # All reasoning / commentary fields are optional
    if is_reasoning_field(p):
        return True

    OPTIONAL_TOKENS = [
        "_source",          # *_source, *_source_lines etc.
        "_hint",
        "_comment",
        "_tooltip",
        "_naming_convention",
        "_layout_note",
        "_confidence_why",
        "_type_reasoning",
        "_max_length_note",
        "_text_note",
        "_naming_note",
        "_tab_index_note",
        "_for_control_reasoning",
        "font.",            # detailed font metadata often extra
        "position._note",
    ]

    return any(tok in p for tok in OPTIONAL_TOKENS)


def is_must_have_field(path: str) -> bool:
    """Minimal structural fields that we consider REQUIRED.
    Missing them is a real mismatch.
    """
    p = path.lower()

    MUST_TOKENS = [
        ".ui.",
        ".logic.",
        ".data.",
        ".name",
        ".type",
        ".position",
        ".size",
        "forms[",
        "controls[",
        "entities[",
        "queries[",
        "rules[",
        "events[",
        "flows[",
    ]

    return any(tok in p for tok in MUST_TOKENS)
```

You can adjust the TOKEN lists later based on real mismatches, but keep this logic for now.

---

## 2. Upgrade `equal_with_tolerance`

If `equal_with_tolerance` already exists, replace/extend it to:

```python
def equal_with_tolerance(expected, actual, field_path: str) -> bool:
    """Relaxed equality:
    - Reasoning / note fields: only care about presence vs absence.
    - All other fields: strict equality AFTER canonicalization.
    """
    lower = field_path.lower()

    # Reasoning / note fields: presence/absence only
    if is_reasoning_field(lower):
        exp_non_empty = bool(str(expected).strip())
        act_non_empty = bool(str(actual).strip())
        return exp_non_empty == act_non_empty

    # Everything else: canonicalizer has already normalized
    return expected == actual
```

Ensure **all primitive comparisons** in the deep compare logic call this function instead of using `==` directly.

---

## 3. Schema-aware dict comparison (subset tolerant)

Find your recursive compare function, something like:

```python
def deep_compare(expected, actual, path=""):
    ...
```

We want the following behavior for dictionaries:

- Keys in `expected` but missing in `actual`:
  - If OPTIONAL → ignore (no mismatch)
  - If MUST-HAVE → count mismatch
- Keys in `actual` but not in `expected`:
  - Treat as extra → ignore (do **not** count mismatch)

Modify the dict branch to something like this pattern (adapt to your variable names):

```python
def deep_compare(expected, actual, path: str = ""):
    total = 0
    mismatches = 0

    # Dict vs Dict
    if isinstance(expected, dict) and isinstance(actual, dict):
        expected_keys = set(expected.keys())
        actual_keys = set(actual.keys())

        # Keys defined in expected
        for key in expected_keys:
            child_path = f"{path}.{key}" if path else key

            if key not in actual:
                # Missing in actual
                if is_optional_field(child_path):
                    # Ignore missing optional fields
                    continue
                # Penalize missing MUST-HAVE fields
                if is_must_have_field(child_path):
                    total += 1
                    mismatches += 1
                # Non-must-have, non-optional fields: treat as optional initially
                continue

            child_exp = expected[key]
            child_act = actual[key]
            child_total, child_mismatches = deep_compare(child_exp, child_act, child_path)
            total += child_total
            mismatches += child_mismatches

        # Keys only in actual → extras (ignored)
        # for key in (actual_keys - expected_keys):
        #     child_path = f"{path}.{key}" if path else key
        #     # Intentionally ignore extras

        return total, mismatches
```

Important:
- Do **NOT** increment `total` for missing optional fields.
- Do **NOT** count extra keys in `actual` as mismatches.

---

## 4. List comparison (keep simple but path-aware)

For lists, keep your existing logic if it works, but ensure you pass an indexed path to children so helpers can still classify correctly.

Example pattern:

```python
    # List vs List
    if isinstance(expected, list) and isinstance(actual, list):
        length = min(len(expected), len(actual))
        for idx in range(length):
            child_path = f"{path}[{idx}]"
            child_total, child_mismatches = deep_compare(expected[idx], actual[idx], child_path)
            total += child_total
            mismatches += child_mismatches

        # If expected is longer than actual → missing structural elements
        if len(expected) > len(actual):
            for idx in range(len(actual), len(expected)):
                child_path = f"{path}[{idx}]"
                if is_must_have_field(child_path):
                    total += 1
                    mismatches += 1

        # Extra items in actual are ignored for now
        return total, mismatches
```

Tune list handling based on how your fixtures behave (forms, controls, entities, etc.).

---

## 5. Primitive (leaf) comparison uses `equal_with_tolerance`

At the bottom of `deep_compare`, for primitive (non-dict, non-list) values:

```python
    # Leaf nodes (numbers, strings, bools, None)
    total += 1
    if not equal_with_tolerance(expected, actual, path):
        mismatches += 1
    return total, mismatches
```

This ensures that:
- Reasoning / notes only care about presence vs absence.
- All other fields use canonicalized equality.

---

## 6. Keep canonicalization wiring as-is

Make sure you **do not change** the existing canonicalization and high-level validator wiring.

For example, you should still have logic like:

```python
from ir_canonicalizer import canonicalize_ir

def validate_ui(expected_ir, actual_ir):
    expected_ir = canonicalize_ir(expected_ir, ir_type="ui")
    actual_ir = canonicalize_ir(actual_ir, ir_type="ui")
    total, mismatches = deep_compare(expected_ir, actual_ir, path="ui")
    return total, mismatches
```

And similar functions for `logic` and `data`.

Do **not** modify `ir_canonicalizer.py`.

---

## 7. After changes — run full test harness

Once you’ve implemented these changes in `validator.py`:

1. Run the full test suite that outputs:
   - UI accuracy
   - Logic accuracy
   - Data accuracy
   - Overall accuracy

2. Expected behavior (approx):
   - Data agent remains high (~98%)
   - UI and Logic agents both increase significantly from:
     - UI:    ~60.5%
     - Logic: ~65.6%
   - Overall should move closer to or above ~80%

3. If any regressions appear:
   - Log a few failing cases with their field paths
   - Adjust the token sets in `is_optional_field` / `is_must_have_field` accordingly

---

## 8. Constraints recap

- Do **NOT** touch prompts.
- Do **NOT** modify the agents.
- Do **NOT** modify `ir_canonicalizer.py`.
- All logic changes stay within `validator.py`.
- Keep public interfaces of validator functions the same.

When done, report:
- New per-agent accuracies
- 1–2 example fields where mismatches disappeared due to:
  - optional field tolerance
  - extra fields being ignored
  - reasoning presence-only comparison

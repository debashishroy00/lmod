#!/usr/bin/env python3
"""
IR Canonicalizer - Normalize IR for Consistent Comparison

WHAT: Normalize both LLM-generated and golden fixture IRs before comparison
WHY: Eliminate noisy value-level drift (notes, reasoning, format differences)
HOW: Walk IR recursively, normalize strings, booleans, numbers, ordering
"""

from __future__ import annotations
from typing import Any, Dict, Union
import re

Json = Dict[str, Any]

SOURCE_LINES_RE = re.compile(r"(\d+)\D+(\d+)")

BOOLEAN_TRUE = {"true", "yes", "y", "1", "enabled", "on"}
BOOLEAN_FALSE = {"false", "no", "n", "0", "disabled", "off"}


def canonicalize_ir(ir: Json, ir_type: str) -> Json:
    """
    Canonicalize an IR dict in-place and return it.
    ir_type: "ui" | "logic" | "data"
    """
    if not isinstance(ir, dict):
        return ir

    _walk_and_normalize(ir)

    if ir_type == "ui":
        _normalize_ui_specific(ir)
    elif ir_type == "logic":
        _normalize_logic_specific(ir)
    elif ir_type == "data":
        _normalize_data_specific(ir)

    return ir


# ----------------- core walker -----------------

def _walk_and_normalize(node: Any, parent_key: str = "") -> Any:
    if isinstance(node, dict):
        for k, v in list(node.items()):
            node[k] = _walk_and_normalize(v, k)
        return node
    elif isinstance(node, list):
        return [_walk_and_normalize(v, parent_key) for v in node]
    elif isinstance(node, str):
        return _normalize_scalar_str(node, parent_key)
    else:
        # numbers, bools, None left as-is
        return node


def _normalize_scalar_str(value: str, field: str) -> Union[str, bool, int, float]:
    v = value.strip()

    # 1) source line ranges → "Lines 12-20"
    if "source_lines" in field.lower():
        m = SOURCE_LINES_RE.search(v)
        if m:
            start, end = m.groups()
            return f"Lines {int(start)}-{int(end)}"
        return v

    # 2) booleans for flag-like fields
    if _looks_like_boolean_field(field):
        low = v.lower()
        if low in BOOLEAN_TRUE:
            return True
        if low in BOOLEAN_FALSE:
            return False

    # 3) numeric-ish → number
    if _looks_like_numeric(v):
        try:
            if "." in v:
                return float(v)
            return int(v)
        except ValueError:
            pass

    # 4) reasoning/note fields → normalized text
    if _looks_like_reasoning_field(field):
        import re as _re

        v = _re.sub(r"\s+", " ", v)
        v = v.strip()
        if v.endswith("."):
            v = v[:-1]
        return v.lower()

    return v


def _looks_like_boolean_field(field: str) -> bool:
    field = field.lower()
    return any(
        token in field
        for token in ["_enabled", "_visible", "_default", "_button", "show_in_taskbar"]
    )


def _looks_like_numeric(v: str) -> bool:
    import re as _re
    return bool(_re.fullmatch(r"-?\d+(\.\d+)?", v))


def _looks_like_reasoning_field(field: str) -> bool:
    field = field.lower()
    return (
        field.endswith("_why")
        or field.endswith("_note")
        or "reason" in field
        or "explain" in field
    )


# -------------- UI-specific --------------------

def _normalize_ui_specific(ir: Json) -> None:
    ui = ir.get("ui")
    if not isinstance(ui, dict):
        return

    # sort controls by name/id for stable ordering
    for collection_key in ["forms", "controls", "textboxes", "labels", "buttons"]:
        col = ui.get(collection_key)
        if isinstance(col, list):
            ui[collection_key] = sorted(
                col,
                key=lambda c: (str(c.get("name") or c.get("id") or "")).lower(),
            )

    # normalize top-level font block if present
    if "font" in ui:
        _normalize_font_block(ui["font"])

    # normalize fonts nested under controls
    for col_val in ui.values():
        if isinstance(col_val, list):
            for ctrl in col_val:
                font = ctrl.get("font")
                if isinstance(font, dict):
                    _normalize_font_block(font)


def _normalize_font_block(font: Dict[str, Any]) -> None:
    weight = str(font.get("weight_source") or font.get("weight") or "").lower()
    bold = str(font.get("bold_derived") or "").lower()

    mapped_weight = "normal"
    if weight in {"bold", "700", "800", "heavy"} or bold in {"true", "yes"}:
        mapped_weight = "bold"

    font["weight_mapping"] = mapped_weight
    font["bold_derived"] = mapped_weight == "bold"


# -------------- Logic-specific -----------------

def _normalize_logic_specific(ir: Json) -> None:
    logic = ir.get("logic")
    if not isinstance(logic, dict):
        return

    for key in ["rules", "events", "flows"]:
        seq = logic.get(key)
        if isinstance(seq, list):
            logic[key] = sorted(
                seq,
                key=lambda r: (str(r.get("name") or r.get("id") or "")).lower(),
            )


# -------------- Data-specific ------------------

def _normalize_data_specific(ir: Json) -> None:
    data = ir.get("data")
    if not isinstance(data, dict):
        return

    for col in ["entities", "queries", "connections"]:
        seq = data.get(col)
        if isinstance(seq, list):
            data[col] = sorted(
                seq,
                key=lambda r: (str(r.get("name") or r.get("id") or "")).lower(),
            )

# Repository Maintenance Scripts

This folder contains one-click scripts for managing the repository.

---

## 🧹 [clean-repo.command](clean-repo.command)

**Double-click to clean the repository**

### What it removes:
- ✅ `output/` - Generated Angular components (~220 KB)
- ✅ `test-migration/` - Test Angular project (~600 MB)
- ✅ `__pycache__/` - Python cache files
- ✅ `.DS_Store` - macOS metadata files
- ✅ `robustness_test_results.json` - Temporary test results

### What it keeps:
- ✅ Source code (`src/`)
- ✅ Sample VB6 files (`samples/`)
- ✅ Documentation (`docs/`, `*.md`)
- ✅ Configuration files
- ✅ Git repository (`.git/`)

**Total space saved**: ~600 MB

---

## 🚀 [git-push.command](git-push.command)

**Double-click to commit and push changes**

### What it does:
1. Shows what changed
2. Asks for confirmation (y/n)
3. Stages all files (`git add .`)
4. Commits with timestamp
5. Pushes to GitHub

**Usage**: Double-click when ready to save your work to GitHub

---

## 📝 Files Protected by .gitignore

The following are automatically ignored by Git and won't be committed:

```
# Never committed to Git
output/                 # Generated code
test-migration/         # Test projects
__pycache__/           # Python cache
*.pyc, *.pyo           # Compiled Python
.DS_Store              # macOS metadata
node_modules/          # Node.js packages
.env                   # API keys (IMPORTANT!)
```

---

## 🔄 Typical Workflow

1. **Work on code** - Make changes to `src/`
2. **Test** - Generate code with `python3 src/codegen/main.py ...`
3. **Clean** - Double-click [clean-repo.command](clean-repo.command) to remove test artifacts
4. **Commit** - Double-click [git-push.command](git-push.command) to save to GitHub

---

## ⚠️ Important Notes

### What to commit:
- ✅ Source code changes
- ✅ Documentation updates
- ✅ Sample VB6 files (in `samples/`)
- ✅ Configuration files

### What NOT to commit:
- ❌ Generated Angular code (`output/`)
- ❌ Test projects (`test-migration/`)
- ❌ API keys (`.env` files)
- ❌ Python cache (`__pycache__/`)
- ❌ Node modules (`node_modules/`)

The `.gitignore` file automatically prevents these from being committed.

---

## 🧪 When to Clean

Clean the repository when:
- You've finished testing and want to free up space
- Before committing to Git (to keep repo size small)
- Repository size is getting large
- You want a fresh start for testing

**Note**: Cleaning is safe - it only removes generated/temporary files, never source code!

---

## 💾 Repository Size

**Before cleanup**: ~600 MB (with test-migration)
**After cleanup**: ~5 MB (source code only)

**Savings**: 99% reduction in size!

---

**Created**: 2025-11-21
**For**: VB6 → Angular Migration Platform

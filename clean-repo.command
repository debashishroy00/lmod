#!/bin/bash

# Repository Cleanup Script
# Double-click this file to clean temporary files and build artifacts

# Change to script directory
cd "$(dirname "$0")"

echo "==============================================="
echo "🧹 Repository Cleanup"
echo "==============================================="
echo ""
echo "📂 Working directory: $(pwd)"
echo ""

echo "This will remove:"
echo "  • output/ directory (generated Angular code)"
echo "  • test-migration/ directory (test Angular project)"
echo "  • Python cache files (__pycache__, *.pyc)"
echo "  • .DS_Store files (macOS)"
echo "  • Temporary test files"
echo ""

read -p "❓ Proceed with cleanup? (y/n): " -n 1 -r
echo ""

if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    echo "❌ Cleanup cancelled"
    echo ""
    read -p "Press Enter to close..."
    exit 1
fi

echo ""
echo "🧹 Cleaning up..."
echo ""

# Remove output directory
if [ -d "output" ]; then
    echo "  🗑️  Removing output/ directory..."
    rm -rf output
    echo "     ✅ Removed output/"
fi

# Remove test-migration directory
if [ -d "test-migration" ]; then
    echo "  🗑️  Removing test-migration/ directory..."
    rm -rf test-migration
    echo "     ✅ Removed test-migration/"
fi

# Remove Python cache
echo "  🗑️  Removing Python cache files..."
find . -type d -name "__pycache__" -exec rm -rf {} + 2>/dev/null
find . -type f -name "*.pyc" -delete 2>/dev/null
find . -type f -name "*.pyo" -delete 2>/dev/null
echo "     ✅ Removed Python cache"

# Remove .DS_Store files
echo "  🗑️  Removing .DS_Store files..."
find . -name ".DS_Store" -delete 2>/dev/null
echo "     ✅ Removed .DS_Store files"

# Remove temp test files
echo "  🗑️  Removing temporary test files..."
rm -f robustness_test_results.json 2>/dev/null
echo "     ✅ Removed temp files"

echo ""
echo "==============================================="
echo "✅ Cleanup complete!"
echo "==============================================="
echo ""

# Show what's left
echo "📊 Repository status:"
du -sh . 2>/dev/null || echo "  (size info unavailable)"
echo ""

echo "Files remaining:"
ls -lh | grep -v "^d" | wc -l | xargs echo "  Files:"
ls -lh | grep "^d" | wc -l | xargs echo "  Directories:"

echo ""
read -p "Press Enter to close..."

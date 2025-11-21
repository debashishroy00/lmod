#!/bin/bash

# One-Click Git Commit & Push Script
# Double-click this file to run!

# Change to script directory
cd "$(dirname "$0")"

echo "==============================================="
echo "🚀 Git Commit & Push Helper"
echo "==============================================="
echo ""
echo "📂 Working directory: $(pwd)"
echo ""

# Check if there are changes
if [[ -z $(git status -s) ]]; then
    echo "✅ No changes to commit. Working directory is clean."
    echo ""
    read -p "Press Enter to close..."
    exit 0
fi

# Show current status
echo "📊 Current changes:"
git status -s
echo ""

# Default commit message with timestamp
TIMESTAMP=$(date "+%Y-%m-%d %H:%M:%S")
COMMIT_MSG="Update: $TIMESTAMP

Changes made:
- Code improvements and updates

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude <noreply@anthropic.com>"

echo "📝 Commit message:"
echo "---"
echo "$COMMIT_MSG"
echo "---"
echo ""

# Ask for confirmation
read -p "❓ Proceed with commit and push? (y/n): " -n 1 -r
echo ""

if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    echo "❌ Aborted by user"
    echo ""
    read -p "Press Enter to close..."
    exit 1
fi

# Stage all changes
echo "📦 Staging all changes..."
git add .

# Commit
echo "💾 Committing..."
git commit -m "$COMMIT_MSG"

# Push
echo "🚀 Pushing to remote..."
git push

echo ""
echo "==============================================="
echo "✅ Success! Changes pushed to GitHub"
echo "==============================================="
echo ""
read -p "Press Enter to close..."

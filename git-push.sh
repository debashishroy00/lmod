#!/bin/bash

# One-Click Git Commit & Push Script
# Usage: ./git-push.sh [optional custom message]

set -e  # Exit on error

echo "==============================================="
echo "🚀 Git Commit & Push Helper"
echo "==============================================="
echo ""

# Check if there are changes
if [[ -z $(git status -s) ]]; then
    echo "✅ No changes to commit. Working directory is clean."
    exit 0
fi

# Show current status
echo "📊 Current changes:"
git status -s
echo ""

# Check if custom message provided
if [ -n "$1" ]; then
    COMMIT_MSG="$1"
else
    # Default commit message with timestamp
    TIMESTAMP=$(date "+%Y-%m-%d %H:%M:%S")
    COMMIT_MSG="Update: $TIMESTAMP

Changes made:
- Code improvements and updates

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude <noreply@anthropic.com>"
fi

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

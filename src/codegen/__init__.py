"""
Angular Code Generation Module

WHAT: Transform IR JSON into Angular 17+ components
WHY: Complete the VB6 → IR → Angular modernization pipeline
HOW: LLM-powered code generation using Claude Sonnet 4
"""

from .angular_generator import AngularGenerator

__all__ = ['AngularGenerator']

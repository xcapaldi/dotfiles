#!/usr/bin/env python3

import json, sys


data = json.load(sys.stdin)

# MODEL — show only when it's not the default model.
# The 1M-context variant's id carries a "[1m]" suffix.
default_model = 'claude-opus-4-8[1m]'
model = data['model']['id']
if model == default_model:
    model = ""
else:
    model = f"[{data['model']['display_name']}]"

# OUTPUT_STYLE — show only when it's not the default style.
default_output_style = 'default'
output_style = data['output_style']['name']
if output_style == default_output_style:
    output_style = ""
else:
    output_style = f'"{output_style}"'

# WORKSPACE.CURRENT_DIR — show only when we've moved out of the project dir.
# workspace_current_dir = data['workspace']['current_dir']
# workspace_project_dir = data['workspace']['project_dir']
# if workspace_current_dir == workspace_project_dir:
#     workspace_current_dir = ""

# CONTEXT WINDOW — show the bar only when usage climbs above 50%.
# "or 0" handles null values.
context_window_used_percentage = int(data.get('context_window', {}).get('used_percentage', 0) or 0)
if context_window_used_percentage > 50:
    filled = context_window_used_percentage * 10 // 100
    context_bar = '[' + '￭' * filled + '･' * (10 - filled) + f'] {context_window_used_percentage}%'
else:
    context_bar = ""

# OUTPUT — join whatever survived; print nothing if no condition is met.
output = " ".join(part for part in (context_bar, model, output_style) if part)
if output:
    print(output)

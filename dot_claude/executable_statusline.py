#!/usr/bin/env python3

import json, sys


data = json.load(sys.stdin)

# MODEL
default_model = 'claude-opus-4-6'
model = data['model']['id']
if model == default_model:
    model = ""
else:
    model = f"[{data['model']['display_name']}] "

# OUTPUT_STYLE
default_output_style = 'default'
output_style = data['output_style']['name']
if output_style == default_output_style:
    output_style = ""
else:
    output_style = f"\"{output_style}\" "

# AGENT
# only populated when claude is run with --agent flag or with agent settings
# claude has a built in way to do this
# try:
#     agent = data['agent']['name']
#     agent = f"[{agent}]"
# except:
#     agent = ""

# WORKSPACE.CURRENT_DIR
# track if we moved dir
workspace_current_dir = data['workspace']['current_dir']
workspace_project_dir = data['workspace']['project_dir']
if workspace_current_dir == workspace_project_dir:
    workspace_current_dir = ""
else:
    workspace_current_dir = f"{workspace_current_dir} "

# CONTEXT_WINDOW.USED_PERCENTAGE
# "or 0" handles null values
context_window_used_percentage = int(data.get('context_window', {}).get('used_percentage', 0) or 0)

# String multiplication builds the bar
filled_context = context_window_used_percentage * 10 // 100
context_bar = '▓' * filled_context + '░' * (10 - filled_context) + f' {context_window_used_percentage}% '

# LINES TOUCHED

lines_added = int(data.get('cost', {}).get('total_lines_added', 0) or 0)
if lines_added == 0:
    lines_added = ""
else:
    lines_added = f"+{lines_added} "

lines_removed = int(data.get('cost', {}).get('total_lines_removed', 0) or 0)
if lines_removed == 0:
    lines_removed = ""
else:
    lines_removed = f"-{lines_removed} "


# DURATION
def fmt_duration(ms):
    s = ms // 1000
    if s < 60:
        return f"{s}s"
    m, s = divmod(s, 60)
    if m < 60:
        return f"{m}m{s:02d}s"
    h, m = divmod(m, 60)
    return f"{h}h{m:02d}m"

total_ms = int(data.get('cost', {}).get('total_duration_ms', 0) or 0)
api_ms = int(data.get('cost', {}).get('total_api_duration_ms', 0) or 0)
if total_ms > 0:
    duration = f"{fmt_duration(total_ms)}({fmt_duration(api_ms)}) "
else:
    duration = ""

# OUTPUT
output = f"{context_bar}{lines_added}{lines_removed}{duration}{model}{output_style}{workspace_current_dir}"
if output != "":
    print(output)

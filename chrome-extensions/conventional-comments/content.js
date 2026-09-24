const LABELS = [
  { name: "praise", desc: "Highlight something positive." },
  { name: "nitpick", desc: "Minor, non-blocking issues (style, naming...)." },
  { name: "suggestion", desc: "Suggest specific improvements." },
  { name: "todo", desc: "Mark something that needs to be done." },
  { name: "issue", desc: "Point out a blocking problem." },
  { name: "question", desc: "Ask for clarification." },
  { name: "thought", desc: "Share a reflection or idea." },
  { name: "chore", desc: "Request a minor, non-code task." },
];

const DECORATIONS = [
  { name: "non-blocking", desc: "Optional change, doesn't block merge." },
  { name: "blocking", desc: "Must be addressed before merge." },
  { name: "if-minor", desc: "Address if the effort is small." },
];

const TEXTAREA_SELECTORS = [
  'textarea[name="comment[body]"]',
  'textarea[name="issue_comment[body]"]',
  'textarea[name="pull_request_review_comment[body]"]',
  'textarea[name="pull_request_review[body]"]',
  'textarea[aria-label="Markdown value"]',
  'textarea[name="note[note]"]',
  'textarea[name="work-item-add-or-edit-comment"]',
];

const UNPROCESSED_QUERY = TEXTAREA_SELECTORS.map(
  (sel) => `${sel}:not([data-cc-toolbar])`,
).join(", ");

const names = (items) => items.map((item) => item.name).join("|");
const PREFIX_REGEX = new RegExp(
  `^\\s*(${names(LABELS)})\\s*(?:\\((${names(DECORATIONS)})\\))?:\\s*`,
);

function parsePrefix(value) {
  const match = value.match(PREFIX_REGEX);
  return {
    length: match ? match[0].length : 0,
    label: match?.[1] ?? "",
    decoration: match?.[2] ?? "",
  };
}

function setPrefix(textarea, label, decoration) {
  const old = parsePrefix(textarea.value);
  const prefix = label ? `${label}${decoration ? `(${decoration})` : ""}: ` : "";
  if (!old.length && !prefix) return;

  const move = (pos) =>
    pos < old.length ? prefix.length : pos - old.length + prefix.length;
  const start = move(textarea.selectionStart);
  const end = move(textarea.selectionEnd);

  // execCommand keeps the edit on the native undo stack and fires the input
  // events that GitHub's React editor listens for.
  textarea.focus();
  textarea.setSelectionRange(0, old.length);
  const edited = prefix
    ? document.execCommand("insertText", false, prefix)
    : document.execCommand("delete");
  if (!edited) {
    textarea.setRangeText(prefix, 0, old.length);
    textarea.dispatchEvent(new Event("input", { bubbles: true }));
  }
  textarea.setSelectionRange(start, end);
}

function makeButton(item, selected, onClick) {
  const button = document.createElement("button");
  button.type = "button";
  button.className = "cc-button";
  button.classList.toggle("cc-selected", selected);
  button.textContent = item.name;
  button.title = item.desc;
  button.addEventListener("click", onClick);
  return button;
}

function render(toolbar, textarea, force = false) {
  const { label, decoration } = parsePrefix(textarea.value);
  const picking = !label || toolbar.dataset.picking === "true";
  const key = `${label}|${decoration}|${picking}`;
  if (!force && toolbar.dataset.key === key) return;
  toolbar.dataset.key = key;
  toolbar.replaceChildren();

  const update = (newLabel, newDecoration) => {
    toolbar.dataset.picking = "false";
    setPrefix(textarea, newLabel, newDecoration);
    render(toolbar, textarea);
  };

  if (picking) {
    for (const item of LABELS) {
      const selected = item.name === label;
      toolbar.append(
        makeButton(item, selected, () =>
          selected ? update("", "") : update(item.name, decoration),
        ),
      );
    }
    return;
  }

  const chip = document.createElement("span");
  chip.className = "cc-label";
  chip.textContent = label;
  chip.title = `Click to change type from '${label}'`;
  chip.addEventListener("click", () => {
    toolbar.dataset.picking = "true";
    render(toolbar, textarea);
  });

  const separator = document.createElement("span");
  separator.className = "cc-separator";
  separator.textContent = ">";

  toolbar.append(chip, separator);
  for (const item of DECORATIONS) {
    const selected = item.name === decoration;
    toolbar.append(
      makeButton(item, selected, () =>
        update(label, selected ? "" : item.name),
      ),
    );
  }
}

function attach(textarea) {
  textarea.dataset.ccToolbar = "true";

  const anchor =
    textarea.closest(
      '[class*="MarkdownInput-module__textArea"], [class*="TextInputBaseWrapper"]',
    ) ?? textarea;
  // GitHub can re-mount the textarea but keep our toolbar next to it.
  if (anchor.previousElementSibling?.classList.contains("cc-toolbar")) {
    anchor.previousElementSibling.remove();
  }

  const toolbar = document.createElement("div");
  toolbar.className = "cc-toolbar";
  anchor.before(toolbar);

  render(toolbar, textarea, true);
  textarea.addEventListener("input", () => render(toolbar, textarea));
}

function scan() {
  document.querySelectorAll(UNPROCESSED_QUERY).forEach(attach);
}

let scanScheduled = false;
new MutationObserver(() => {
  if (scanScheduled) return;
  scanScheduled = true;
  setTimeout(() => {
    scanScheduled = false;
    scan();
  }, 100);
}).observe(document.body, { childList: true, subtree: true });

scan();

const vscode = require("vscode");

function activate(context) {
  const runCmd = vscode.commands.registerCommand("one.runFile", () => {
    const editor = vscode.window.activeTextEditor;
    if (!editor) {
      vscode.window.showErrorMessage("No active ONE file.");
      return;
    }

    const file = editor.document.fileName;
    const terminal = vscode.window.createTerminal("ONE");
    terminal.show();

    // HARD-CODED compiler path (for now, to remove ambiguity)
    const compiler =
      "C:\\Users\\Max Shoener\\one-dev\\one-compiler\\one.js";

    terminal.sendText(`node "${compiler}" "${file}"`);
  });

  context.subscriptions.push(runCmd);
}

function deactivate() { }

module.exports = { activate, deactivate };

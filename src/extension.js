const vscode = require("vscode");
const path = require("path");

function activate(context) {
  const runCommand = vscode.commands.registerCommand(
    "one.runFile",
    () => {
      const editor = vscode.window.activeTextEditor;
      if (!editor) {
        vscode.window.showErrorMessage("No active ONE file.");
        return;
      }

      const filePath = editor.document.fileName;
      const terminal = vscode.window.createTerminal("ONE");
      terminal.show();

      terminal.sendText(`node ONE-Language/compiler/one.js"${filePath}"`);
    }
  );

  context.subscriptions.push(runCommand);
}

function deactivate() {}

module.exports = { activate, deactivate };

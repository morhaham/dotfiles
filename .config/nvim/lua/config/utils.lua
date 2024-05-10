-- Function to create an auto-command
local M = {}

function M.map(modes, lhs, rhs, opts)
  opts = opts or {}
  opts.silent = opts.silent ~= false
  if opts.remap and not vim.g.vscode then
    opts.remap = nil
  end
  vim.keymap.set(modes, lhs, rhs, opts)
end

return M

local map = require("config.utils").map

local M = {}

M.attachLspKeymaps = function(ev)
  -- Enable completion triggered by <c-x><c-o>
  vim.bo[ev.buf].omnifunc = "v:lua.vim.lsp.omnifunc"

  -- Buffer local mappings.
  -- See `:help vim.lsp.*` for documentation on any of the below functions
  local opts = { buffer = ev.buf }
  vim.keymap.set("n", "gD", vim.lsp.buf.declaration, vim.tbl_extend("force", opts, { desc = "Go to declaration" }))
  vim.keymap.set("n", "gd", vim.lsp.buf.definition, vim.tbl_extend("force", opts, { desc = "Go to definition" }))
  vim.keymap.set("n", "K", vim.lsp.buf.hover, vim.tbl_extend("force", opts, { desc = "Show symbol information" }))
  vim.keymap.set(
    "n",
    "gi",
    vim.lsp.buf.implementation,
    vim.tbl_extend("force", opts, { desc = "Go to implementation" })
  )
  vim.keymap.set(
    "n",
    "gt",
    vim.lsp.buf.type_definition,
    vim.tbl_extend("force", opts, { desc = "Go to type definition" })
  )
  vim.keymap.set("n", "<leader>rn", vim.lsp.buf.rename, vim.tbl_extend("force", opts, { desc = "Rename symbol" }))
  vim.keymap.set(
    { "n", "v" },
    "<leader>ca",
    vim.lsp.buf.code_action,
    vim.tbl_extend("force", opts, { desc = "Code action" })
  )
  vim.keymap.set(
    { "n", "v" },
    "gr",
    vim.lsp.buf.references,
    vim.tbl_extend("force", opts, { desc = "References to quickfix list" })
  )
  vim.keymap.set(
    "n",
    "<leader>q",
    vim.lsp.diagnostic.set_loclist,
    vim.tbl_extend("force", opts, { desc = "Show diagnostics" })
  )
end

return M

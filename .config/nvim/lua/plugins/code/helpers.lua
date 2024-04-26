local map = require("config.utils").map

local M = {}

M.attachLspKeymaps = function(ev)
  -- Enable completion triggered by <c-x><c-o>
  vim.bo[ev.buf].omnifunc = "v:lua.vim.lsp.omnifunc"

  -- Buffer local mappings.
  -- See `:help vim.lsp.*` for documentation on any of the below functions
  local opts = { buffer = ev.buf }
  map(
    "n",
    "gD",
    vim.lsp.buf.declaration,
    vim.tbl_extend("force", opts, { desc = "Go to declaration" })
  )
  map(
    "n",
    "gd",
    vim.lsp.buf.definition,
    vim.tbl_extend("force", opts, { desc = "Go to definition" })
  )
  map(
    "n",
    "K",
    vim.lsp.buf.hover,
    vim.tbl_extend("force", opts, { desc = "Show symbol information" })
  )
  map(
    "n",
    "gi",
    vim.lsp.buf.implementation,
    vim.tbl_extend("force", opts, { desc = "Go to implementation" })
  )

  map(
    "n",
    "gt",
    vim.lsp.buf.type_definition,
    vim.tbl_extend("force", opts, { desc = "Go to type definition" })
  )
  map(
    "n",
    "<leader>rn",
    vim.lsp.buf.rename,
    vim.tbl_extend("force", opts, { desc = "Rename symbol" })
  )
  map(
    { "n", "v" },
    "<leader>ca",
    vim.lsp.buf.code_action,
    vim.tbl_extend("force", opts, { desc = "Code action" })
  )
  map(
    { "n", "v" },
    "gr",
    vim.lsp.buf.references,
    vim.tbl_extend("force", opts, { desc = "References to quickfix list" })
  )
end

return M

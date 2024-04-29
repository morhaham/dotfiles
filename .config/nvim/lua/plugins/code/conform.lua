return {
  "stevearc/conform.nvim",
  event = { "BufWritePre" },
  cmd = { "ConformInfo" },
  keys = {
    {
      "<C-f>",
      '<cmd>lua require("conform").format({ async = true, lsp_fallback = false })<CR>',
      mode = "",
      desc = "Format buffer",
    },
  },
  -- Everything in opts will be passed to setup()
  opts = {
    events = { "BufWritePre" },
    -- Define your formatters
    formatters_by_ft = {
      lua = { "stylua" },
      javascriptreact = { "prettier" },
      typescriptreact = { "prettier" },
      javascript = { "prettier" },
      typescript = { "prettier" },
    },
    -- Set up format-on-save
    format_on_save = { timeout_ms = 500, lsp_fallback = false },
  },
}

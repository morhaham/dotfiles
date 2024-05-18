return {
  "stevearc/conform.nvim",
  event = { "BufWritePre" },
  cmd = { "ConformInfo" },
  config = function(_, opts)
    local conform = require("conform")

    vim.keymap.set("n", "<C-f>", function()
      conform.format({ async = true })
    end, { desc = "Format buffer" })

    conform.setup(opts)
  end,
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
    -- format_on_save = { timeout_ms = 500, lsp_fallback = false },
  },
}

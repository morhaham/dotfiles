return {
  "mfussenegger/nvim-lint",
  event = { "VeryLazy" },
  config = function(_, opts)
    local lint = require("lint")
    lint.linters_by_ft = opts.linters_by_ft
    vim.api.nvim_create_autocmd(opts.events, {
      group = vim.api.nvim_create_augroup("nvim-lint", { clear = true }),
      callback = function()
        lint.try_lint(nil, { ignore_errors = true })
      end,
    })
  end,
  opts = function()
    local linters = { eslint = { "eslint" } }
    return {
      events = { "BufWritePost" },
      linters_by_ft = {
        javascript = linters.eslint,
        typescript = linters.eslint,
        javascriptreact = linters.eslint,
        typescriptreact = linters.eslint,
      },
    }
  end,
}

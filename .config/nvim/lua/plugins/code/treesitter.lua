return {
  {
    "nvim-treesitter/nvim-treesitter",
    dependencies = {
      "OXY2DEV/markview.nvim",
    },
    build = ":TSUpdate",
    event = { "BufEnter" },
    config = function()
      local configs = require("nvim-treesitter.configs")
      configs.setup({
        ensure_installed = {
          "go",
          "lua",
          "vim",
          "vimdoc",
          "typescript",
          "tsx",
          "javascript",
          "html",
        },
        sync_install = false,
        highlight = { enable = true },
        indent = { enable = true },
      })
    end,
  },
}

return {
  {
    "JoosepAlviste/nvim-ts-context-commentstring",
    name = "nvim-ts-context-commentstring",
    dependencies = { "Comment.nvim" },
    opts = {
      enable_autocmd = false,
    },
  },
  {
    "numToStr/Comment.nvim",
    name = "Comment.nvim",
    config = function()
      require("Comment").setup({
        pre_hook = require(
          "ts_context_commentstring.integrations.comment_nvim"
        ).create_pre_hook(),
        opleader = {
          line = "<C-_>", -- since tmux maps <C-/> to <C-_>
          block = "<C-b>",
        },
        extra = {
          eol = "<C-_>", -- since tmux maps <C-/> to <C-_>
        },
      })
    end,
  },
}

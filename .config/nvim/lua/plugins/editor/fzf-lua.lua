return {
  "ibhagwan/fzf-lua",
  -- optional for icon support
  dependencies = { "nvim-tree/nvim-web-devicons" },
  opts = function()
    local actions = require("fzf-lua").actions
    return {
      keymap = {
        fzf = {
          ["ctrl-a"] = "toggle-all",
        },
      },
      actions = {
        files = {
          ["default"] = actions.file_edit_or_qf,
          ["ctrl-s"] = actions.file_split,
          ["ctrl-v"] = actions.file_vsplit,
          ["ctrl-t"] = actions.file_tabedit,
          ["ctrl-q"] = actions.file_sel_to_qf,
          ["ctrl-l"] = actions.file_sel_to_ll,
        },
        buffers = {
          -- providers that inherit these actions:
          --   buffers, tabs, lines, blines
          ["default"] = actions.buf_edit,
          ["ctrl-s"] = actions.buf_split,
          ["ctrl-v"] = actions.buf_vsplit,
          ["ctrl-t"] = actions.buf_tabedit,
        },
      },
    }
  end,
  keys = {
    {
      "<leader>ff",
      "<cmd>lua require('fzf-lua').files({ resume = true, fd_opts = '--color=never --type f --hidden --follow --no-ignore' })<CR>",
      desc = "Find files",
    },
    {
      "<leader><leader>",
      "<cmd>lua require('fzf-lua').git_files({ resume = true })<CR>",
      desc = "Find git files",
    },
    {
      "<leader>fb",
      '<cmd>lua require("fzf-lua").buffers()<CR>',
      desc = "Find buffers",
    },
    {
      "<leader>fq",
      '<cmd>lua require("fzf-lua").quickfix()<CR>',
      desc = "Find in quickfix list",
    },
    {
      "<leader>fl",
      '<cmd>lua require("fzf-lua").loclist()<CR>',
      desc = "Find in location list",
    },
    {
      "<leader>fa",
      '<cmd>lua require("fzf-lua").args()<CR>',
      desc = "Find in argument list",
    },
    {
      "<leader>/",
      "<cmd>lua require('fzf-lua').live_grep({ resume = true })<CR>",
      desc = "Live grep",
    },
    {
      "<leader><C-_>", -- since tmux maps <C-/> to <C-_>
      "<cmd>lua require('fzf-lua').live_grep({ cmd = [[rg --color=always --smart-case -uuu ]] })<CR>",
      desc = "Live grep no ignores",
    },
  },
}

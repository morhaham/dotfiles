return {
  {
    "rose-pine/neovim",
    name = "rose-pine",
    priority = 1000,
    lazy = false,
    config = function()
      require("rose-pine").setup({
        highlight_groups = {
          StatusLineTerm = { fg = "subtle", bg = "surface" },
          StatusLineTermNC = { fg = "muted", bg = "surface", blend = 0 },
        },
        styles = {
          transparency = true,
          italic = true,
          bold= true,
        },
      })
      vim.cmd("colorscheme rose-pine")
    end,
  },
  {
    "sainnhe/gruvbox-material",
    lazy = false,
    priority = 1000,
    config = function()
      -- Optionally configure and load the colorscheme
      -- directly inside the plugin declaration.
      -- vim.g.gruvbox_material_background = "hard"
      -- vim.g.gruvbox_material_foreground = "material"
      -- vim.g.gruvbox_material_enable_italic = true
      -- vim.g.gruvbox_material_visual = "reverse"
      -- vim.cmd.colorscheme("gruvbox-material")
    end,
  },
  {
    "Mofiqul/vscode.nvim",
    name = "vscode",
    priority = 1000,
    -- config = function()
    --   require("vscode").setup({
    --     transparent = true,
    --   })
    --   vim.cmd("colorscheme vscode")
    -- end,
  },
  {
    "projekt0n/github-nvim-theme",
    lazy = false, -- make sure we load this during startup if it is your main colorscheme
    priority = 1000, -- make sure to load this before all the other start plugins
    -- config = function()
    --   require("github-theme").setup({
    --     -- options = {
    --     --   transparent = true,
    --     -- },
    --   })
    --
    --   vim.cmd("colorscheme github_dark_default")
    -- end,
  },
  {
    "aktersnurra/no-clown-fiesta.nvim",
    name = "no-clown-fiesta",
    priority = 1000,
    -- config = function()
    --   vim.cmd("colorscheme no-clown-fiesta")
    -- end,
  },
  {
    "alligator/accent.vim",
    priority = 1000,
  },
  {
    "p00f/alabaster.nvim",
    priority = 1000,
    --[[ config = function()
      vim.cmd("colorscheme alabaster")
    end, ]]
  },
  {
    "nyoom-engineering/oxocarbon.nvim",
    priority = 1000,
    --[[ config = function()
      vim.cmd("colorscheme oxocarbon")
    end, ]]
  },
  {
    "navarasu/onedark.nvim",
    priority = 1000,
    config = function()
      -- require("onedark").setup({ style = "darker" })
      -- require("onedark").load()
    end,
  },
  {
    "rebelot/kanagawa.nvim",
    priority = 1000,
    --[[ config = function()
      vim.cmd("colorscheme kanagawa-wave")
    end, ]]
  },
  {
    "catppuccin/nvim",
    priority = 1000,
    --[[ config = function()
      vim.cmd("colorscheme catppuccin-mocha")
    end, ]]
  },
  {
    "dasupradyumna/midnight.nvim",
    lazy = false,
    priority = 1000,
    --[[ config = function()
      vim.cmd("colorscheme midnight")
    end, ]]
  },
  {
    "Mofiqul/dracula.nvim",
    lazy = false,
    priority = 1000,
    --[[ config = function()
      vim.cmd("colorscheme dracula")
    end, ]]
  },
  {
    "whatyouhide/vim-gotham",
    lazy = false,
    priority = 1000,
    --[[ config = function()
      vim.cmd("colorscheme gotham")
    end, ]]
  },
}

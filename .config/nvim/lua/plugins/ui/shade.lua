return {
  "sunjon/shade.nvim",
  config = function()
    require("shade").setup({
      overlay_opacity = 40, -- Adjust how dark the inactive panes get
      opacity_step = 1,
      -- Tell shade to completely ignore these window/buffer types:
      exclude_filetypes = {
        "NvimTree",
        "neo-tree",
        "qf", -- Quickfix window
        "lualine", -- If your statusline uses a dedicated buffer type
        "prompt",
      },
    })
  end,
}

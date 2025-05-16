return {
  {
    "lukas-reineke/indent-blankline.nvim",
    main = "ibl",
    -- config = function()
    --   local hooks = require("ibl.hooks")
    --   hooks.register(hooks.type.HIGHLIGHT_SETUP, function()
    --     vim.api.nvim_set_hl(0, "ScopeIndentColor", { fg = "#d3d3d3" })
    --   end)
    --   -- hooks.register(hooks.type.SCOPE_HIGHLIGHT, function(hl)
    --   --   hl.hl = "RainbowRed"
    --   -- end)
    --   require("ibl").setup({
    --     scope = {
    --       enabled = true,
    --       show_start = false,
    --       show_end = false,
    --       highlight = {
    --         "ScopeIndentColor",
    --       },
    --     },
    --   })
    -- end,
    ---@module "ibl"
    ---@type ibl.config
    opts = {
      scope = {
        enabled = true,
        show_start = false,
        show_end = false,
        -- highlight = {
        --   "RainbowDelimiterRed",
        --   "RainbowDelimiterYellow",
        --   "RainbowDelimiterBlue",
        --   "RainbowDelimiterOrange",
        --   "RainbowDelimiterGreen",
        --   "RainbowDelimiterViolet",
        --   "RainbowDelimiterCyan",
        -- },
      },
    },
  },
}

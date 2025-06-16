return {
  "nvim-lualine/lualine.nvim",
  event = "ColorScheme",
  config = function()
    require("lualine").setup({
      options = {
        -- @usage 'rose-pine' | 'rose-pine-alt'
        theme = "rose-pine",
        -- theme = 'vscode',
        section_separators = "",
        component_separators = "",
        disabled_filetypes = { "TelescopePrompt" },
      },
      extensions = { "oil" },
      sections = {
        -- lualine_a = {
        --   -- {
        --   --   "mode",
        --   --   fmt = function(str)
        --   --     return "[" .. str .. "]"
        --   --   end,
        --   -- }
        --   ,
        -- },
        lualine_a = { "filename" },
        lualine_b = { "branch" },
        lualine_c = { "" },
        lualine_x = { "filetype", "encoding" },
        lualine_y = { "" },
        lualine_z = { "location" },
      },
    })
  end,
}

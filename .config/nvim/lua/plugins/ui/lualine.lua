local diagnostics_icons = require("config.icons").diagnostics
return {
  "nvim-lualine/lualine.nvim",
  event = "ColorScheme",
  config = function()
    require("lualine").setup({
      options = {
        theme = "tokyonight",
        -- theme = 'vscode',
        section_separators = { left = "", right = "" },
        component_separators = { left = "", right = "" },
        disabled_filetypes = { "TelescopePrompt" },
      },
      sections = {
        lualine_a = {
          {
            "mode",
            fmt = function(str)
              -- return "[" .. str .. "]"
              return str
            end,
            -- padding = { left = 0 },
          },
        },
        lualine_b = {
          { "filename", path = 1 },
        },
        lualine_c = { "branch" },
        lualine_x = { "filetype", "encoding" },
        lualine_y = {
          {
            "diagnostics",
            sources = { "nvim_diagnostic" },
            symbols = {
              error = diagnostics_icons.error,
              warn = diagnostics_icons.warn,
              info = diagnostics_icons.info,
              hint = diagnostics_icons.hint,
            },
          },
          {
            "diff",
            colored = true,
            symbols = { added = "+", modified = "~", removed = "-" },
          },
        },
        lualine_z = {
          {
            "location",
          },
          {
            "progress",
            -- padding = { right = 0 },
          },
        },
      },
    })
  end,
}

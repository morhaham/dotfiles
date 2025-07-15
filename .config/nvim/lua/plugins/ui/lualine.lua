local diagnostics_icons = require("config.icons").diagnostics
local git_icons = require("config.icons").git

return {
  "nvim-lualine/lualine.nvim",
  event = "ColorScheme",
  config = function()
    require("lualine").setup({
      options = {
        theme = "tokyonight",
        -- theme = 'vscode',
        -- section_separators = { left = "", right = "" },
        -- section_separators = { left = "/", right = "/" },
        -- component_separators = { left = "", right = "" },
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
          { "branch", icon = git_icons.branch },
        },
        lualine_c = {
          { "filename", path = 1 },
        },
        lualine_x = { "filetype", "encoding" },
        lualine_y = {
          {
            "diagnostics",
            sources = { "nvim_diagnostic" },
            symbols = {
              error = diagnostics_icons.error .. " ",
              warn = diagnostics_icons.warn .. " ",
              info = diagnostics_icons.info .. " ",
              hint = diagnostics_icons.hint .. " ",
            },
          },
          {
            "diff",
            colored = true,
            symbols = {
              added = git_icons.add .. " ",
              modified = git_icons.change .. " ",
              removed = git_icons.delete .. " ",
            },
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

return {
  "glepnir/galaxyline.nvim",
  dependencies = { "nvin-tree/nvim-web-devicons" },
  branch = "main",
  config = function()
    local galaxyline = require("galaxyline")
    local fileinfo = require("galaxyline.provider_fileinfo")
    local buffer = require("galaxyline.provider_buffer")
    local vcs = require("galaxyline.provider_vcs")
    local diagnostic = require("galaxyline.provider_diagnostic")
    local colors = {
      -- rose-pine colors: https://rosepinetheme.com/palette/ingredients/
      base = "#191724",
      surface = "#1f1d2e",
      overlay = "#26233a",
      muted = "#6e6a86",
      subtle = "#908caa",
      text = "#e0def4",
      love = "#eb6f92",
      gold = "#f6c177",
      rose = "#ebbcba",
      pine = "#31748f",
      foam = "#9ccfd8",
      iris = "#c4a7e7",
      highlight_low = "#21202e",
      highlight_med = "#403d52",
      highlight_high = "#524f67",
    }
    local lspclient = require("galaxyline.provider_lsp")
    galaxyline.section.left[0] = {
      FilePath = {
        provider = function()
          return fileinfo.get_current_file_path("●", "")
        end,
        highlight = { colors.text, colors.highlight_low },
        separator = "|",
        separator_highlight = { nil, colors.highlight_low },
      },
    }
    galaxyline.section.left[1] = {
      GitBranch = {
        provider = vcs.get_git_branch,
        icon = "  ",
        highlight = { colors.gold, colors.highlight_low },
        separator = "  ",
        separator_highlight = { nil, colors.highlight_low },
      },
    }
    local cond_sep = function()
      return ""
      -- if diagnostic.get_diagnostic_error() ~= nil then
      --   return " | "
      -- else
      --   return ""
      -- end
    end
    galaxyline.section.right[0] = {
      DiagnosticError = {
        provider = diagnostic.get_diagnostic_error,
        icon = " ",
        highlight = {
          colors.love,
          colors.highlight_low,
        },
        separator = cond_sep(),
        separator_highlight = { nil, colors.highlight_low },
      },
    }
    galaxyline.section.right[1] = {
      DiagnosticWarn = {
        provider = diagnostic.get_diagnostic_warn,
        icon = " ",
        highlight = {
          colors.gold,
          colors.highlight_low,
        },
        separator = cond_sep(), -- separator = "",
        separator_highlight = { nil, colors.highlight_low },
      },
    }
    galaxyline.section.right[2] = {
      LspClient = {
        provider = lspclient.get_lsp_client,
        icon = fileinfo.get_file_icon,
        highlight = {
          require("galaxyline.provider_fileinfo").get_file_icon_color,
          colors.highlight_low,
        },
        separator = "| ",
        separator_highlight = { nil, colors.highlight_low },
      },
    }
    galaxyline.section.right[3] = {
      LineColumn = {
        provider = fileinfo.line_column,
        highlight = { colors.text, colors.highlight_low },
        separator = " |",
        separator_highlight = { nil, colors.highlight_low },
      },
    }
  end,
}

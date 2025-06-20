return {
  "folke/trouble.nvim",
  opts = {}, -- for default options, refer to the configuration section for custom setup.
  cmd = "Trouble",
  dependencies = {
    "nvim-tree/nvim-web-devicons", -- optional, for file icons
  },
  keys = {
    {
      "<leader>xx",
      "<cmd>Trouble diagnostics toggle<cr>",
      desc = "Diagnostics (Trouble)",
    },
    {
      "<leader>xX",
      "<cmd>Trouble diagnostics toggle filter.buf=0<cr>",
      desc = "Buffer Diagnostics (Trouble)",
    },
    {
      "<leader>cs",
      "<cmd>Trouble symbols toggle focus=false<cr>",
      desc = "Symbols (Trouble)",
    },
    {
      "<leader>cl",
      "<cmd>Trouble lsp toggle focus=false win.position=right<cr>",
      desc = "LSP Definitions / references / ... (Trouble)",
    },
    {
      "<leader>xL",
      "<cmd>Trouble loclist toggle<cr>",
      desc = "Location List (Trouble)",
    },
    {
      "<leader>xQ",
      "<cmd>Trouble qflist toggle<cr>",
      desc = "Quickfix List (Trouble)",
    },
    {
      "gr",
      "<cmd>Trouble lsp_references<cr>",
      desc = "References (Trouble)",
    },
    {
      "gD",
      "<cmd>Trouble lsp_definitions<cr>",
      desc = "Go to definition (Trouble)",
    },
    {
      "gd",
      "<cmd>Trouble lsp_declarations<cr>",
      desc = "Go to declaration (Trouble)",
    },
    {
      "gt",
      "<cmd>Trouble lsp_type_definitions<cr>",
      desc = "Go to type definition (Trouble)",
    },
    {
      "gi",
      "<cmd>Trouble lsp_implementations<cr>",
      desc = "Go to implementation (Trouble)",
    },
  },
}

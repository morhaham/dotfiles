local attachLspKeymaps = require("plugins.code.helpers").attachLspKeymaps

return {
  {
    "williamboman/mason.nvim",
    config = function()
      require("mason").setup()
    end,
  },
  {
    "williamboman/mason-lspconfig.nvim",
    dependencies = { "williamboman/mason.nvim", "lspconfig" },
    config = function(_, opts)
      require("mason-lspconfig").setup(opts)
    end,
    opts = function()
      local lspconfig = require("lspconfig")
      local lsp_capabilities = require("cmp_nvim_lsp").default_capabilities()
      local default_setup = function(server)
        lspconfig[server].setup({
          capabilities = lsp_capabilities,
        })
      end
      return {
        ensure_installed = {
          -- "tsserver",
          "tailwindcss",
          "gopls",
          "golangci_lint_ls",
          "templ",
        },
        handlers = { default_setup },
      }
    end,
  },
  {
    "neovim/nvim-lspconfig",
    name = "lspconfig",
    config = function()
      vim.api.nvim_create_autocmd("LspAttach", {
        group = vim.api.nvim_create_augroup("UserLspConfig", { clear = true }),
        callback = attachLspKeymaps,
      })
    end,
    keys = {
      {
        "<leader>df",
        "<cmd>lua vim.diagnostic.open_float()<CR>",
        desc = "Show diagnostic float",
      },
      {
        "<leader>d[",
        "<cmd>lua vim.diagnostic.goto_prev()<CR>",
        desc = "Diagnostic prev",
      },
      {
        "<leader>d]",
        "<cmd>lua vim.diagnostic.goto_next()<CR>",
        desc = "Diagnostic next",
      },
      {
        "<leader>dl",
        "<cmd>lua vim.diagnostic.setloclist()<CR>",
        desc = "Diagnostic location list",
      },
    },
  },
  {
    "pmizio/typescript-tools.nvim",
    name = "typescript-tools",
    dependencies = { "nvim-lua/plenary.nvim", "neovim/nvim-lspconfig" },
    opts = {
      expose_as_code_action = { "all" },
    },
    keys = {
      {
        "<leader>oi",
        "<cmd>TSToolsOrganizeImports<cr>",
        "Organize imports",
      },
    },
  },
}

return {
  {
    "mason-org/mason-lspconfig.nvim",
    dependencies = {
      { "mason-org/mason.nvim", opts = {} },
      "lspconfig",
    },
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
          "tailwindcss",
          "gopls",
          "golangci_lint_ls",
          "templ",
          "ts_ls",
          "lua_ls",
          "pylsp",
          "ts_ls",
          "cssls",
          "html",
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
        callback = function(ev)
          -- Enable completion triggered by <c-x><c-o>
          vim.bo[ev.buf].omnifunc = "v:lua.vim.lsp.omnifunc"
          vim.diagnostic.config({ virtual_text = false })

          -- Buffer local mappings.
          -- See `:help vim.lsp.*` for documentation on any of the below functions
          local opts = { buffer = ev.buf }
          vim.keymap.set(
            "n",
            "gD",
            vim.lsp.buf.declaration,
            vim.tbl_extend("force", opts, { desc = "Go to declaration" })
          )
          vim.keymap.set(
            "n",
            "gd",
            vim.lsp.buf.definition,
            vim.tbl_extend("force", opts, { desc = "Go to definition" })
          )
          vim.keymap.set(
            "n",
            "K",
            vim.lsp.buf.hover,
            vim.tbl_extend("force", opts, { desc = "Show symbol information" })
          )
          vim.keymap.set(
            "n",
            "gi",
            vim.lsp.buf.implementation,
            vim.tbl_extend("force", opts, { desc = "Go to implementation" })
          )
          vim.keymap.set(
            "n",
            "gt",
            vim.lsp.buf.type_definition,
            vim.tbl_extend("force", opts, { desc = "Go to type definition" })
          )
          vim.keymap.set(
            "n",
            "<leader>cr",
            vim.lsp.buf.rename,
            vim.tbl_extend("force", opts, { desc = "Rename symbol" })
          )
          vim.keymap.set(
            { "n", "v" },
            "<leader>ca",
            vim.lsp.buf.code_action,
            vim.tbl_extend("force", opts, { desc = "Code action" })
          )
          vim.keymap.set(
            "n",
            "<M-k>",
            vim.diagnostic.open_float,
            vim.tbl_extend("force", opts, { desc = "Show diagnostic float" })
          )
          vim.keymap.set(
            "n",
            "<M-[>",
            vim.diagnostic.goto_prev,
            vim.tbl_extend("force", opts, { desc = "Diagnostic prev" })
          )
          vim.keymap.set(
            "n",
            "<M-]>",
            vim.diagnostic.goto_next,
            vim.tbl_extend("force", opts, { desc = "Diagnostic next" })
          )
          vim.keymap.set(
            "n",
            "<M-'>",
            vim.diagnostic.setloclist,
            vim.tbl_extend("force", opts, { desc = "Diagnostic location list" })
          )
        end,
      })
    end,
  },
  {
    "jay-babu/mason-nvim-dap.nvim",
    dependencies = { "mason-org/mason.nvim" },
    cmd = { "DapInstall", "DapUninstall" },
    opts = {
      automatic_installation = true,
      handlers = {},
      ensure_installed = {
        "delve",
        "js",
      },
    },
  },
  {
    "rachartier/tiny-inline-diagnostic.nvim",
    event = "VeryLazy",
    priority = 1000,
    config = function()
      require("tiny-inline-diagnostic").setup()
    end,
  },
  {
    "rmagatti/goto-preview",
    dependencies = { "rmagatti/logger.nvim" },
    event = "BufEnter",
    config = function()
      require("goto-preview").setup({
        default_mappings = true,
      })
      vim.keymap.set("n", "gp", "", { desc = "Goto preview definition" })
    end,
  },
}

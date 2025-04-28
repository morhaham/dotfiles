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
          "tailwindcss",
          "gopls",
          "golangci_lint_ls",
          "templ",
          "tsserver",
          "lua_ls",
          "pylsp",
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
            "<leader>rn",
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
            { "n", "v" },
            "gr",
            vim.lsp.buf.references,
            vim.tbl_extend("force", opts, { desc = "References to quickfix list" })
          )
          vim.keymap.set(
            "n",
            "<M-;>",
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
  -- {
  --   "pmizio/typescript-tools.nvim",
  --   name = "typescript-tools",
  --   dependencies = { "nvim-lua/plenary.nvim", "neovim/nvim-lspconfig" },
  --   config = function(_, opts)
  --     local ts_tools = require("typescript-tools")
  --     vim.keymap.set("n", "<leader>oi", ":TSToolsOrganizeImports<CR>", { desc = "Organize imports" })
  --     ts_tools.setup(opts)
  --   end,
  --   opts = {
  --     expose_as_code_action = { "all" },
  --   },
  -- },
}

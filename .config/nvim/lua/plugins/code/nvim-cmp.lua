return {
  {
    "hrsh7th/nvim-cmp",
    name = "nvim-cmp",
    dependencies = {
      "saadparwaiz1/cmp_luasnip",
      "hrsh7th/cmp-nvim-lsp",
      "luasnip",
      "hrsh7th/cmp-nvim-lsp-signature-help",
    },
    config = function()
      local cmp = require("cmp")
      local luasnip = require("luasnip")

      cmp.setup({
        sources = cmp.config.sources({
          { name = "nvim_lsp_signature_help" },
          { name = "luasnip" },
          { name = "nvim_lsp" },
          { name = "buffer" },
        }),
        preselect = cmp.PreselectMode.None,
        mapping = {
          ["<CR>"] = cmp.mapping.confirm({ select = true }),
          ["<C-n>"] = cmp.mapping(function(fallback)
            if cmp.visible() then
              cmp.select_next_item()
            else
              fallback()
            end
          end),
          ["<C-p>"] = cmp.mapping(function(fallback)
            if cmp.visible() then
              cmp.select_prev_item()
            else
              fallback()
            end
          end),
          ["<Tab>"] = cmp.mapping(function(fallback)
            if luasnip.locally_jumpable(1) then
              luasnip.jump(1)
            else
              fallback()
            end
          end, { "i", "s" }),
          ["<S-Tab>"] = cmp.mapping(function(fallback)
            if luasnip.locally_jumpable(-1) then
              luasnip.jump(-1)
            else
              fallback()
            end
          end, { "i", "s" }),
        },
        enabled = function()
          -- disable completion in comments
          local context = require("cmp.config.context")
          -- keep command mode completion enabled when cursor is in a comment
          if vim.api.nvim_get_mode().mode == "c" then
            return true
          else
            return not context.in_treesitter_capture("comment") and not context.in_syntax_group("Comment")
          end
        end,
        snippet = {
          expand = function(args)
            return luasnip.lsp_expand(args.body)
          end,
        },
      })
    end,
  },
  {
    "hrsh7th/cmp-nvim-lsp",
    name = "cmp-nvim-lsp",
  },
  {
    "saadparwaiz1/cmp_luasnip",
    name = "cmp_luasnip",
    dependencies = { "nvim-cmp" },
  },
  {
    "hrsh7th/cmp-nvim-lsp-signature-help",
  },
}

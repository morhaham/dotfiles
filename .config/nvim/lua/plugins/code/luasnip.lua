return {
  {
    "L3MON4D3/LuaSnip",
    name = "luasnip",
    dependencies = { "friendly-snippets" },
    config = function()
      -- HACK: Cancel the snippet session when leaving insert mode.
      local luasnip = require("luasnip")
      local unlink_group = vim.api.nvim_create_augroup("UnlinkSnippet", {})
      vim.api.nvim_create_autocmd("ModeChanged", {
        group = unlink_group,
        -- when going from select mode to normal and when leaving insert mode
        pattern = { "s:n", "i:*" },
        callback = function(event)
          if
            luasnip.session
            and luasnip.session.current_nodes[event.buf]
            and not luasnip.session.jump_active
          then
            luasnip.unlink_current()
          end
        end,
      })
      require("luasnip.loaders.from_vscode").lazy_load()
    end,
  },
  {
    "rafamadriz/friendly-snippets",
    name = "friendly-snippets",
  },
}

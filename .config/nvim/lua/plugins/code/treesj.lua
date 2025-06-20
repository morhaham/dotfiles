return {
  'Wansmer/treesj',
  dependencies = { 'nvim-treesitter/nvim-treesitter' }, -- if you install parsers with `nvim-treesitter`
  config = function()
    require('treesj').setup({})
  end,
  keys = {
    { '<space>cj', function() require('treesj').toggle() end, desc = 'Toggle Join/Split' },
  },
}

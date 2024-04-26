return {
  "AckslD/nvim-neoclip.lua",
  dependencies = {
    { "ibhagwan/fzf-lua" },
  },
  config = function()
    require("neoclip").setup()
  end,
  -- keys = {
  --   {"<C-p>", function ()
  --     require('neoclip').paste(nil, true)
  --   end, },
  --   {"<C-n>", function ()
  --     require('neoclip').paste(nil, false)
  --   end}
  -- }
}

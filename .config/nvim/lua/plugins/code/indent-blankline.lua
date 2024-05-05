return {
  {
    "lukas-reineke/indent-blankline.nvim",
    main = "ibl",
    opts = {},
    config = function()
      local ibl = require("ibl")
      ibl.setup({
        scope = {
          show_start = false,
          show_end = false,
        },
      })
    end,
  },
}

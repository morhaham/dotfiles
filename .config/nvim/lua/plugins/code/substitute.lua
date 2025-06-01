return {
  "gbprod/substitute.nvim",
  config = function(_, opts)
    local substitute = require("substitute")
    substitute.setup(opts)
    vim.keymap.set("n", "<leader>s", require("substitute").operator, { noremap = true, desc = "Substitute operator" })
    vim.keymap.set("n", "<leader>ss", require("substitute").line, { noremap = true, desc = "Substitute line" })
    vim.keymap.set("n", "<leader>S", require("substitute").eol, { noremap = true , desc = "Substitute end of line" })
    vim.keymap.set("x", "<leader>s", require("substitute").visual, { noremap = true, desc = "Substitute visual selection" })
  end,
  opts = {},
}

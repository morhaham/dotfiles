return {
  "gbprod/substitute.nvim",
  config = function(_, opts)
    local substitute = require("substitute")
    substitute.setup(opts)
    vim.keymap.set("n", "<leader>s", require("substitute").operator, { noremap = true, desc = "Substitute operator" })
    vim.keymap.set("n", "<leader>ss", require("substitute").line, { noremap = true, desc = "Substitute line" })
    vim.keymap.set("n", "<leader>S", require("substitute").eol, { noremap = true, desc = "Substitute end of line" })
    vim.keymap.set(
      "x",
      "<leader>s",
      require("substitute").visual,
      { noremap = true, desc = "Substitute visual selection" }
    )

    -- Use substitute.nvim to replace word under cursor with input text
    vim.keymap.set("v", "<leader>cr", function()
      local input = vim.fn.input("Replace with: ")
      if input == "" then
        return
      end
      -- Save input to register (substitute.nvim uses this)
      vim.fn.setreg("s", input)
      -- Use substitute.nvim to replace selection with register s
      require("substitute").visual({
       register = "s",
      })
    end, { desc = "Substitute visual with input text" })

  end,
  opts = {},
}

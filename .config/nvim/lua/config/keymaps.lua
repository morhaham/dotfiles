vim.keymap.set("i", "<Esc><BS>", "<C-w>", { desc = "(mac specific) delete word backwards with option backsapce" })
vim.keymap.set({ "i", "n" }, "<C-s>", "<ESC><cmd>w<CR>", { desc = "Save current buffer" })
vim.keymap.set("n", "<ESC>", "<cmd>noh<CR><ESC>", { desc = "Escape to clear search highlight" })
vim.keymap.set({ "n", "v" }, "<Leader>q", "<cmd>q<CR>", { desc = "Quit buffer" })
vim.keymap.set({ "n" }, "<Leader>e", "<cmd>e %:h<CR>", { desc = "Open file explorer" })
vim.keymap.set({ "n" }, "<Leader>n", ":cnext<CR>", {
  desc = "Go to the next item in the quickfix list",
})
vim.keymap.set({ "n" }, "<Leader>p", ":cprev<CR>", {
  desc = "Go to the prev item in the quickfix list",
})
vim.keymap.set("i", "kk", "<ESC>", { desc = "Exit insert mode with kk" })

vim.keymap.set({ "n", "v" }, "<Leader>bf", function()
  local conform = require("conform")
  conform.format({ async = false })
end, { desc = "Format buffer" })

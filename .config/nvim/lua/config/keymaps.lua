local map = require("config.utils").map
map("i", "<Esc><BS>", "<C-w>", { desc = "(mac specific) delete word backwards with option backsapce" })
map({ "i", "n" }, "<C-s>", "<ESC><cmd>w<CR>", { desc = "Save current buffer" })
map("n", "<ESC>", "<cmd>noh<CR><ESC>", { desc = "Escape to clear search highlight" })
map({ "n", "v" }, "<leader>q", "<cmd>q<CR>", { desc = "Quit buffer" })
map({ "n" }, "<leader>e", "<cmd>e %:h<CR>", { desc = "Open file explorer" })
map({ "n" }, "<leader>n", ":cnext<CR>", {
  desc = "Go to the next item in the quickfix list",
})
map({ "n" }, "<leader>p", ":cprev<CR>", {
  desc = "Go to the prev item in the quickfix list",
})
map("i", "kk", "<ESC>", { desc = "Exit insert mode with kj" })

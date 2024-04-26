local map = require("config.utils").map

map("n", "<C-l>", "<C-w>l", { desc = "Move to split on the right" })
map("n", "<C-h>", "<C-w>h", { desc = "Move to split on the left" })
map("n", "<C-j>", "<C-w>j", { desc = "Move to the split below" })
map("n", "<C-k>", "<C-w>k", { desc = "Move to the split above" })
map(
  "i",
  "<Esc><BS>",
  "<C-w>",
  { desc = "(mac specific) delete word backwards with option backsapce" }
)
map({ "i", "n" }, "<C-s>", "<ESC><cmd>w<CR>", { desc = "Save current buffer" })
map(
  "n",
  "<ESC>",
  "<cmd>noh<CR><ESC>",
  { desc = "Escape to clear search highlight" }
)
map({ "n", "v" }, "<leader>q", "<cmd>q<CR>", { desc = "Quit buffer" })
map({ "n" }, "<leader>e", "<cmd>e %:h<CR>", { desc = "Open file explorer" })
map({ "n" }, "<leader>n", ":cnext<CR>", {
  desc = "Go to the next item in the quickfix list",
})
map({ "n" }, "<leader>p", ":cprev<CR>", {
  desc = "Go to the prev item in the quickfix list",
})

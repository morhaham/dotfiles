vim.keymap.set("n", "<ESC>", "<cmd>noh<CR><ESC>", { desc = "Escape to clear search highlight" })
vim.keymap.set({ "n", "v" }, "<leader>q", "<cmd>q<CR>", { desc = "Quit buffer" })
vim.keymap.set({ "i", "n", "v" }, "<C-s>", "<ESC><cmd>w<CR>", { desc = "Save current buffer" })


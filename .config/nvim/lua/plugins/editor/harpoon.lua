return {
  "ThePrimeagen/harpoon",
  branch = "harpoon2",
  dependencies = { "nvim-lua/plenary.nvim", "telescope.nvim" },
  config = function()
    local harpoon = require("harpoon")
    -- harpoon:extend({
    --   UI_CREATE = function(cx)
    --     vim.keymap.set("n", "<C-v>", function()
    --       harpoon.ui:select_menu_item({ vsplit = true })
    --     end, { buffer = cx.bufnr })
    --
    --     vim.keymap.set("n", "<C-x>", function()
    --       harpoon.ui:select_menu_item({ split = true })
    --     end, { buffer = cx.bufnr })
    --
    --     vim.keymap.set("n", "<C-s>", function()
    --       require("harpoon"):save()
    --     end)
    --   end,
    -- })

    local conf = require("telescope.config").values
    local function toggle_telescope(harpoon_files)
      local file_paths = {}
      for _, item in ipairs(harpoon_files.items) do
        table.insert(file_paths, item.value)
      end

      require("telescope.pickers")
        .new(require("telescope.themes").get_dropdown({ previewer = false }), {
          prompt_title = "Harpoon",
          finder = require("telescope.finders").new_table({
            results = file_paths,
          }),
          previewer = conf.file_previewer({}),
          sorter = conf.generic_sorter({}),
        })
        :find()
    end

    vim.keymap.set("n", "<C-e>", function()
      toggle_telescope(harpoon:list())
    end, { desc = "Open harpoon window" })

    vim.keymap.set("n", "<C-a>", function()
      harpoon:list():add()
    end)
    vim.keymap.set("n", "<C-n>", function()
      harpoon:list():next()
    end)
    vim.keymap.set("n", "<C-p>", function()
      harpoon:list():prev()
    end)
    harpoon.setup()
  end,
}

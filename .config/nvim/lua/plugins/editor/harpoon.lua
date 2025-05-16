return {
  "ThePrimeagen/harpoon",
  branch = "harpoon2",
  dependencies = { "nvim-lua/plenary.nvim", "telescope.nvim" },
  config = function()
    local harpoon = require("harpoon")
    harpoon:extend({
      UI_CREATE = function(cx)
        vim.keymap.set("n", "<C-v>", function()
          harpoon.ui:select_menu_item({ vsplit = true })
        end, { buffer = cx.bufnr })

        vim.keymap.set("n", "<C-x>", function()
          harpoon.ui:select_menu_item({ split = true })
        end, { buffer = cx.bufnr })

        vim.keymap.set("n", "<C-s>", function()
          require("harpoon"):save()
        end)
      end,
    })

    local conf = require("telescope.config").values
    local function toggle_telescope(harpoon_files)
      local file_paths = {}
      for _, item in ipairs(harpoon_files.items) do
        table.insert(file_paths, item.value)
      end

      local make_finder = function()
        local paths = {}

        for _, item in ipairs(harpoon_files.items) do
          table.insert(paths, item.value)
        end

        return require("telescope.finders").new_table({
          results = paths,
        })
      end

      require("telescope.pickers")
        .new(
          require("telescope.themes").get_dropdown({
            previewer = false,
          }),
          {
            prompt_title = "Harpoon",
            finder = require("telescope.finders").new_table({
              results = file_paths,
            }),
            previewer = conf.file_previewer({}),
            sorter = conf.generic_sorter({}),
            attach_mappings = function(prompt_buffer_number, map)
              map("i", "<C-d>", function()
                local state = require("telescope.actions.state")
                local selected_entry = state.get_selected_entry()
                local current_picker = state.get_current_picker(prompt_buffer_number)

                harpoon:list():remove(selected_entry)
                current_picker:refresh(make_finder())
              end)

              return true
            end,
          }
        )
        :find()
    end
    

    harpoon:setup()
    vim.keymap.set("n", "<Leader>h", function()
      toggle_telescope(harpoon:list())
    end, { desc = "Open harpoon window" })
    vim.keymap.set("n", "<Leader>a", function()
      harpoon:list():add()
    end, { desc = "Add harpoon" })

    vim.keymap.set("n", "<Leader>1", function()
      harpoon:list():select(1)
    end, { desc = "Select harpoon 1" })
    vim.keymap.set("n", "<Leader>2", function()
      harpoon:list():select(2)
    end, { desc = "Select harpoon 2" })
    vim.keymap.set("n", "<Leader>3", function()
      harpoon:list():select(3)
    end, { desc = "Select harpoon 3" })
    vim.keymap.set("n", "<Leader>4", function()
      harpoon:list():select(4)
    end, { desc = "Select harpoon 4" })

    vim.keymap.set("n", "<M-n>", function()
      harpoon:list():next()
    end, { desc = "Next harpoon" })
    vim.keymap.set("n", "<M-p>", function()
      harpoon:list():prev()
    end, { desc = "Previous harpoon" })
  end,
}

return {
  {
    "kevinhwang91/nvim-ufo",
    dependencies = {
      "kevinhwang91/promise-async",
      "luukvbaal/statuscol.nvim",
    },
    lazy = false,
    opts = {
      -- INFO: Uncomment to use treeitter as fold provider, otherwise nvim lsp is used
      -- provider_selector = function(bufnr, filetype, buftype)
      --   return { "treesitter", "indent" }
      -- end,
      open_fold_hl_timeout = 400,
      close_fold_kinds_for_ft = { default = { "imports", "comment" } },

      preview = {
        mappings = {
          scrollU = "<C-u>",
          scrollD = "<C-d>",
          jumpTop = "[",
          jumpBot = "]",
        },
      },
    },
    init = function()
      vim.o.fillchars = [[eob: ,fold: ,foldopen:,foldsep: ,foldclose:]]
      vim.o.foldcolumn = "1"
      vim.o.foldlevel = 99 -- Using ufo provider need a large value, feel free to decrease the value
      vim.o.foldlevelstart = 99
      vim.o.foldenable = true
    end,
    config = function(_, opts)
      local builtin = require("statuscol.builtin")

      require("statuscol").setup({
        relculright = true,
        segments = {
          -- Simulate the sign column while not showing the gitsigns
          {
            sign = {
              name = { ".*" },
              text = { ".*" },
            },
            click = "v:lua.ScSa",
          },
          {
            sign = {
              namespace = { "gitsigns" },
              colwidth = 1,
              wrap = true,
              foldclosed = true,
            },
            condition = {
              function(args)
                return vim.wo[args.win].number or vim.b[args.buf].gitsigns_status
              end,
            },
            click = "v:lua.ScSa",
          },
          -- Show the fold column with custom icons
          {
            text = {
              function(args)
                args.fold.close = ""
                args.fold.open = ""
                args.fold.sep = " "
                return builtin.foldfunc(args)
              end,
            },
            click = "v:lua.ScFa",
          },
          -- Simulate the line number column without the right padding
          {
            text = { builtin.lnumfunc, " " },
            click = "v:lua.ScLa",
          },
        },
      })

      local handler = function(virtText, lnum, endLnum, width, truncate)
        local newVirtText = {}
        local totalLines = vim.api.nvim_buf_line_count(0)
        local foldedLines = endLnum - lnum
        local suffix = (" 󰦸 %d %d%%"):format(foldedLines, foldedLines / totalLines * 100)
        local sufWidth = vim.fn.strdisplaywidth(suffix)
        local targetWidth = width - sufWidth
        local curWidth = 0
        for _, chunk in ipairs(virtText) do
          local chunkText = chunk[1]
          local chunkWidth = vim.fn.strdisplaywidth(chunkText)
          if targetWidth > curWidth + chunkWidth then
            table.insert(newVirtText, chunk)
          else
            chunkText = truncate(chunkText, targetWidth - curWidth)
            local hlGroup = chunk[2]
            table.insert(newVirtText, { chunkText, hlGroup })
            chunkWidth = vim.fn.strdisplaywidth(chunkText)
            -- str width returned from truncate() may less than 2nd argument, need padding
            if curWidth + chunkWidth < targetWidth then
              suffix = suffix .. (" "):rep(targetWidth - curWidth - chunkWidth)
            end
            break
          end
          curWidth = curWidth + chunkWidth
        end
        local rAlignAppndx = math.max(math.min(vim.opt.textwidth["_value"], width - 1) - curWidth - sufWidth, 0)
        suffix = (" "):rep(rAlignAppndx) .. suffix
        table.insert(newVirtText, { suffix, "MoreMsg" })
        return newVirtText
      end

      opts["fold_virt_text_handler"] = handler
      require("ufo").setup(opts)
      vim.keymap.set("n", "zR", require("ufo").openAllFolds)
      vim.keymap.set("n", "zM", require("ufo").closeAllFolds)
      vim.keymap.set("n", "zr", require("ufo").openFoldsExceptKinds)
      vim.keymap.set("n", "zk", require("ufo").peekFoldedLinesUnderCursor, { desc = "Peek folded lines under cursor" })
    end,
  },
}

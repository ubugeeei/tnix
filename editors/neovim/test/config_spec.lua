local config = require("tnix.config")
local plugin = require("tnix")

local function assert_equal(actual, expected, label)
  if not vim.deep_equal(actual, expected) then
    error(string.format("%s\nexpected: %s\nactual: %s", label, vim.inspect(expected), vim.inspect(actual)))
  end
end

local function with_temp_tree(files, run)
  local root = vim.fn.tempname()
  vim.fn.mkdir(root, "p")

  for relative, content in pairs(files) do
    local path = root .. "/" .. relative
    vim.fn.mkdir(vim.fn.fnamemodify(path, ":h"), "p")
    vim.fn.writefile(vim.split(content, "\n", { plain = true }), path)
  end

  local ok, result = pcall(run, root)
  vim.fn.delete(root, "rf")
  if not ok then
    error(result)
  end
  return result
end

assert_equal(config.normalize_cmd(nil), { "tnix-lsp" }, "normalize_cmd falls back to tnix-lsp")
assert_equal(config.normalize_cmd(" custom-lsp "), { "custom-lsp" }, "normalize_cmd trims string commands")
assert_equal(config.normalize_cmd({ "custom-lsp", "", " --stdio " }), { "custom-lsp", "--stdio" }, "normalize_cmd compacts argv lists")
assert_equal(config.root_markers(nil), { "flake.nix", "cabal.project", "pnpm-workspace.yaml", ".git" }, "root_markers exposes the default workspace markers")
assert_equal(config.root_markers({ "", "flake.nix", "custom.marker" }), { "flake.nix", "custom.marker" }, "root_markers drops blank entries")

with_temp_tree({
  ["flake.nix"] = "{}",
  ["packages/app/main.tnix"] = "1",
  ["custom.marker"] = "",
}, function(root)
  local nested = root .. "/packages/app/main.tnix"
  assert_equal(config.resolve_root_dir(nested, {}), root, "resolve_root_dir finds workspace markers")
  assert_equal(config.resolve_root_dir(nested, { root_markers = { "custom.marker" } }), root, "resolve_root_dir honors custom markers")
  assert_equal(config.resolve_root_dir(nested, { root_dir = root .. "/packages" }), root .. "/packages", "resolve_root_dir honors explicit roots")
  assert_equal(config.resolve_root_dir(nested, { root_dir = function() return root .. "/via-callback" end }), root .. "/via-callback", "resolve_root_dir honors callback roots")

  assert_equal(
    config.server_config(nested, { cmd = { "tnix-lsp", "--stdio" }, cmd_env = { TNIX_ENV = "1" }, name = "tnix-custom" }),
    {
      name = "tnix-custom",
      cmd = { "tnix-lsp", "--stdio" },
      cmd_env = { TNIX_ENV = "1" },
      root_dir = root,
    },
    "server_config assembles lsp.start options"
  )
end)

plugin.setup({})
assert_equal(vim.filetype.match({ filename = "demo.tnix" }), "tnix", "setup registers the .tnix filetype")
assert_equal(vim.filetype.match({ filename = "demo.d.tnix" }), "tnix", "setup registers the .d.tnix filetype")

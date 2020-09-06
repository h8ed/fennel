package.preload["fennel.repl"] = package.preload["fennel.repl"] or function(...)
  local utils = require("fennel.utils")
  local parser = require("fennel.parser")
  local compiler = require("fennel.compiler")
  local specials = require("fennel.specials")
  local function default_read_chunk(parser_state)
    local function _0_()
      if (0 < parser_state.stackSize) then
        return ".."
      else
        return ">> "
      end
    end
    io.write(_0_())
    io.flush()
    local input = io.read()
    return (input and (input .. "\n"))
  end
  local function default_on_values(xs)
    io.write(table.concat(xs, "\9"))
    return io.write("\n")
  end
  local function default_on_error(errtype, err, lua_source)
    local function _1_()
      local _0_0 = errtype
      if (_0_0 == "Lua Compile") then
        return ("Bad code generated - likely a bug with the compiler:\n" .. "--- Generated Lua Start ---\n" .. lua_source .. "--- Generated Lua End ---\n")
      elseif (_0_0 == "Runtime") then
        return (compiler.traceback(err, 4) .. "\n")
      else
        local _ = _0_0
        return ("%s error: %s\n"):format(errtype, tostring(err))
      end
    end
    return io.write(_1_())
  end
  local save_source = table.concat({"local ___i___ = 1", "while true do", " local name, value = debug.getlocal(1, ___i___)", " if(name and name ~= \"___i___\") then", " ___replLocals___[name] = value", " ___i___ = ___i___ + 1", " else break end end"}, "\n")
  local function splice_save_locals(env, lua_source)
    env.___replLocals___ = (env.___replLocals___ or {})
    local spliced_source = {}
    local bind = "local %s = ___replLocals___['%s']"
    for line in lua_source:gmatch("([^\n]+)\n?") do
      table.insert(spliced_source, line)
    end
    for name in pairs(env.___replLocals___) do
      table.insert(spliced_source, 1, bind:format(name, name))
    end
    if ((1 < #spliced_source) and (spliced_source[#spliced_source]):match("^ *return .*$")) then
      table.insert(spliced_source, #spliced_source, save_source)
    end
    return table.concat(spliced_source, "\n")
  end
  local function completer(env, scope, text)
    local matches = {}
    local input_fragment = text:gsub(".*[%s)(]+", "")
    local function add_partials(input, tbl, prefix)
      for k in utils.allpairs(tbl) do
        local k0 = nil
        if ((tbl == env) or (tbl == env.___replLocals___)) then
          k0 = scope.unmanglings[k]
        else
          k0 = k
        end
        if ((#matches < 2000) and (type(k0) == "string") and (input == k0:sub(0, #input))) then
          table.insert(matches, (prefix .. k0))
        end
      end
      return nil
    end
    local function add_matches(input, tbl, prefix)
      local prefix0 = nil
      if prefix then
        prefix0 = (prefix .. ".")
      else
        prefix0 = ""
      end
      if not input:find("%.") then
        return add_partials(input, tbl, prefix0)
      else
        local head, tail = input:match("^([^.]+)%.(.*)")
        local raw_head = nil
        if ((tbl == env) or (tbl == env.___replLocals___)) then
          raw_head = scope.manglings[head]
        else
          raw_head = head
        end
        if (type(tbl[raw_head]) == "table") then
          return add_matches(tail, tbl[raw_head], (prefix0 .. head))
        end
      end
    end
    add_matches(input_fragment, (scope.specials or {}))
    add_matches(input_fragment, (scope.macros or {}))
    add_matches(input_fragment, (env.___replLocals___ or {}))
    add_matches(input_fragment, env)
    add_matches(input_fragment, (env._ENV or env._G or {}))
    return matches
  end
  local function repl(options)
    local old_root_options = utils.root.options
    local env = nil
    if options.env then
      env = utils.wrapEnv(options.env)
    else
      env = setmetatable({}, {__index = (_G._ENV or _G)})
    end
    local save_locals_3f = ((options.saveLocals ~= false) and env.debug and env.debug.getlocal)
    local opts = {}
    local _ = nil
    for k, v in pairs(options) do
      opts[k] = v
    end
    _ = nil
    local read_chunk = (opts.readChunk or default_read_chunk)
    local on_values = (opts.onValues or default_on_values)
    local on_error = (opts.onError or default_on_error)
    local pp = (opts.pp or tostring)
    local byte_stream, clear_stream = parser.granulate(read_chunk)
    local chars = {}
    local read, reset = nil, nil
    local function _1_(parser_state)
      local c = byte_stream(parser_state)
      chars[(#chars + 1)] = c
      return c
    end
    read, reset = parser.parser(_1_)
    local scope = compiler.makeScope()
    opts.useMetadata = (options.useMetadata ~= false)
    if (opts.allowedGlobals == nil) then
      opts.allowedGlobals = specials.currentGlobalNames(opts.env)
    end
    if opts.registerCompleter then
      local function _3_(...)
        return completer(env, scope, ...)
      end
      opts.registerCompleter(_3_)
    end
    local function loop()
      for k in pairs(chars) do
        chars[k] = nil
      end
      local ok, parse_ok_3f, x = pcall(read)
      local src_string = string.char((_G.unpack or table.unpack)(chars))
      utils.root.options = opts
      if not ok then
        on_error("Parse", parse_ok_3f)
        clear_stream()
        reset()
        return loop()
      else
        if parse_ok_3f then
          do
            local _4_0, _5_0 = pcall(compiler.compile, x, {["assert-compile"] = opts["assert-compile"], ["parse-error"] = opts["parse-error"], correlate = opts.correlate, moduleName = opts.moduleName, scope = scope, source = src_string, useMetadata = opts.useMetadata})
            if ((_4_0 == false) and (nil ~= _5_0)) then
              local msg = _5_0
              clear_stream()
              on_error("Compile", msg)
            elseif ((_4_0 == true) and (nil ~= _5_0)) then
              local source = _5_0
              local source0 = nil
              if save_locals_3f then
                source0 = splice_save_locals(env, source)
              else
                source0 = source
              end
              local lua_ok_3f, loader = pcall(specials.loadCode, source0, env)
              if not lua_ok_3f then
                clear_stream()
                on_error("Lua Compile", loader, source0)
              else
                local _7_0, _8_0 = nil, nil
                local function _9_()
                  return {loader()}
                end
                local function _10_(...)
                  return on_error("Runtime", ...)
                end
                _7_0, _8_0 = xpcall(_9_, _10_)
                if ((_7_0 == true) and (nil ~= _8_0)) then
                  local ret = _8_0
                  env._ = ret[1]
                  env.__ = ret
                  on_values(utils.map(ret, pp))
                end
              end
            end
          end
          utils.root.options = old_root_options
          return loop()
        end
      end
    end
    return loop()
  end
  return repl
end
package.preload["fennel.specials"] = package.preload["fennel.specials"] or function(...)
  local utils = require("fennel.utils")
  local parser = require("fennel.parser")
  local compiler = require("fennel.compiler")
  local unpack = (_G.unpack or table.unpack)
  local SPECIALS = compiler.scopes.global.specials
  local function wrapEnv(env)
    local function _0_(_, key)
      if (type(key) == "string") then
        return env[compiler.globalUnmangling(key)]
      else
        return env[key]
      end
    end
    local function _1_(_, key, value)
      if (type(key) == "string") then
        env[compiler.globalMangling(key)] = value
        return nil
      else
        env[key] = value
        return nil
      end
    end
    local function _2_()
      local function putenv(k, v)
        local _3_
        if (type(k) == "string") then
          _3_ = compiler.globalUnmangling(k)
        else
          _3_ = k
        end
        return _3_, v
      end
      return next, utils.kvmap(env, putenv), nil
    end
    return setmetatable({}, {__index = _0_, __newindex = _1_, __pairs = _2_})
  end
  local function currentGlobalNames(env)
    return utils.kvmap((env or _G), compiler.globalUnmangling)
  end
  local function loadCode(code, environment, filename)
    local environment0 = ((environment or _ENV) or _G)
    if (_G.setfenv and _G.loadstring) then
      local f = assert(_G.loadstring(code, filename))
      _G.setfenv(f, environment0)
      return f
    else
      return assert(load(code, filename, "t", environment0))
    end
  end
  local function doc_2a(tgt, name)
    if not tgt then
      return (name .. " not found")
    else
      local docstring = (((compiler.metadata):get(tgt, "fnl/docstring") or "#<undocumented>")):gsub("\n$", ""):gsub("\n", "\n  ")
      if (type(tgt) == "function") then
        local arglist = table.concat(((compiler.metadata):get(tgt, "fnl/arglist") or {"#<unknown-arguments>"}), " ")
        local _0_
        if (#arglist > 0) then
          _0_ = " "
        else
          _0_ = ""
        end
        return string.format("(%s%s%s)\n  %s", name, _0_, arglist, docstring)
      else
        return string.format("%s\n  %s", name, docstring)
      end
    end
  end
  local function docSpecial(name, arglist, docstring)
    compiler.metadata[SPECIALS[name]] = {["fnl/arglist"] = arglist, ["fnl/docstring"] = docstring}
    return nil
  end
  local function compileDo(ast, scope, parent, start)
    local start0 = (start or 2)
    local len = #ast
    local subScope = compiler.makeScope(scope)
    for i = start0, len do
      compiler.compile1(ast[i], subScope, parent, {nval = 0})
    end
    return nil
  end
  SPECIALS["do"] = function(ast, scope, parent, opts, start, chunk, subScope, preSyms)
    local start0 = (start or 2)
    local subScope0 = (subScope or compiler.makeScope(scope))
    local chunk0 = (chunk or {})
    local len = #ast
    local outerTarget = opts.target
    local outerTail = opts.tail
    local retexprs = {returned = true}
    if (not outerTarget and (opts.nval ~= 0) and not outerTail) then
      if opts.nval then
        local syms = {}
        for i = 1, opts.nval, 1 do
          local s = ((preSyms and preSyms[i]) or compiler.gensym(scope))
          syms[i] = s
          retexprs[i] = utils.expr(s, "sym")
        end
        outerTarget = table.concat(syms, ", ")
        compiler.emit(parent, ("local %s"):format(outerTarget), ast)
        compiler.emit(parent, "do", ast)
      else
        local fname = compiler.gensym(scope)
        local fargs = ((scope.vararg and "...") or "")
        compiler.emit(parent, ("local function %s(%s)"):format(fname, fargs), ast)
        retexprs = utils.expr((fname .. "(" .. fargs .. ")"), "statement")
        outerTail = true
        outerTarget = nil
      end
    else
      compiler.emit(parent, "do", ast)
    end
    if (len < start0) then
      compiler.compile1(nil, subScope0, chunk0, {tail = outerTail, target = outerTarget})
    else
      for i = start0, len do
        local subopts = {nval = (((i ~= len) and 0) or opts.nval), tail = (((i == len) and outerTail) or nil), target = (((i == len) and outerTarget) or nil)}
        utils.propagateOptions(opts, subopts)
        local subexprs = compiler.compile1(ast[i], subScope0, chunk0, subopts)
        if (i ~= len) then
          compiler.keepSideEffects(subexprs, parent, nil, ast[i])
        end
      end
    end
    compiler.emit(parent, chunk0, ast)
    compiler.emit(parent, "end", ast)
    return retexprs
  end
  docSpecial("do", {"..."}, "Evaluate multiple forms; return last value.")
  SPECIALS.values = function(ast, scope, parent)
    local len = #ast
    local exprs = {}
    for i = 2, len do
      local subexprs = compiler.compile1(ast[i], scope, parent, {nval = ((i ~= len) and 1)})
      exprs[(#exprs + 1)] = subexprs[1]
      if (i == len) then
        for j = 2, #subexprs, 1 do
          exprs[(#exprs + 1)] = subexprs[j]
        end
      end
    end
    return exprs
  end
  docSpecial("values", {"..."}, "Return multiple values from a function. Must be in tail position.")
  SPECIALS.fn = function(ast, scope, parent)
    local index, fnName, isLocalFn, docstring = 2, utils.isSym(ast[2])
    local fScope = nil
    do
      local _0_0 = compiler.makeScope(scope)
      _0_0["vararg"] = false
      fScope = _0_0
    end
    local fChunk = {}
    local multi = (fnName and utils.isMultiSym(fnName[1]))
    compiler.assert((not multi or not multi.multiSymMethodCall), ("unexpected multi symbol " .. tostring(fnName)), ast[index])
    if (fnName and (fnName[1] ~= "nil")) then
      isLocalFn = not multi
      if isLocalFn then
        fnName = compiler.declareLocal(fnName, {}, scope, ast)
      else
        fnName = compiler.symbolToExpression(fnName, scope)[1]
      end
      index = (index + 1)
    else
      isLocalFn = true
      fnName = compiler.gensym(scope)
    end
    do
      local argList = nil
      local function _2_()
        if (type(ast[index]) == "table") then
          return ast[index]
        else
          return ast
        end
      end
      argList = compiler.assert(utils.isTable(ast[index]), "expected parameters", _2_())
      local function getArgName(i, name)
        if utils.isVarg(name) then
          compiler.assert((i == #argList), "expected vararg as last parameter", ast[2])
          fScope.vararg = true
          return "..."
        elseif (utils.isSym(name) and (utils.deref(name) ~= "nil") and not utils.isMultiSym(utils.deref(name))) then
          return compiler.declareLocal(name, {}, fScope, ast)
        elseif utils.isTable(name) then
          local raw = utils.sym(compiler.gensym(scope))
          local declared = compiler.declareLocal(raw, {}, fScope, ast)
          compiler.destructure(name, raw, ast, fScope, fChunk, {declaration = true, nomulti = true})
          return declared
        else
          return compiler.assert(false, ("expected symbol for function parameter: %s"):format(tostring(name)), ast[2])
        end
      end
      local argNameList = utils.kvmap(argList, getArgName)
      if ((type(ast[(index + 1)]) == "string") and ((index + 1) < #ast)) then
        index = (index + 1)
        docstring = ast[index]
      end
      for i = (index + 1), #ast, 1 do
        compiler.compile1(ast[i], fScope, fChunk, {nval = (((i ~= #ast) and 0) or nil), tail = (i == #ast)})
      end
      if isLocalFn then
        compiler.emit(parent, ("local function %s(%s)"):format(fnName, table.concat(argNameList, ", ")), ast)
      else
        compiler.emit(parent, ("%s = function(%s)"):format(fnName, table.concat(argNameList, ", ")), ast)
      end
      compiler.emit(parent, fChunk, ast)
      compiler.emit(parent, "end", ast)
      if utils.root.options.useMetadata then
        local args = nil
        local function _5_(v)
          if utils.isTable(v) then
            return "\"#<table>\""
          else
            return ("\"%s\""):format(tostring(v))
          end
        end
        args = utils.map(argList, _5_)
        local metaFields = {"\"fnl/arglist\"", ("{" .. table.concat(args, ", ") .. "}")}
        if docstring then
          table.insert(metaFields, "\"fnl/docstring\"")
          table.insert(metaFields, ("\"" .. docstring:gsub("%s+$", ""):gsub("\\", "\\\\"):gsub("\n", "\\n"):gsub("\"", "\\\"") .. "\""))
        end
        local metaStr = ("require(\"%s\").metadata"):format((utils.root.options.moduleName or "fennel"))
        compiler.emit(parent, ("pcall(function() %s:setall(%s, %s) end)"):format(metaStr, fnName, table.concat(metaFields, ", ")))
      end
    end
    return utils.expr(fnName, "sym")
  end
  docSpecial("fn", {"name?", "args", "docstring?", "..."}, "Function syntax. May optionally include a name and docstring.\nIf a name is provided, the function will be bound in the current scope.\nWhen called with the wrong number of args, excess args will be discarded\nand lacking args will be nil, use lambda for arity-checked functions.")
  SPECIALS.lua = function(ast, _, parent)
    compiler.assert(((#ast == 2) or (#ast == 3)), "expected 1 or 2 arguments", ast)
    if (ast[2] ~= nil) then
      table.insert(parent, {ast = ast, leaf = tostring(ast[2])})
    end
    if (#ast == 3) then
      return tostring(ast[3])
    end
  end
  SPECIALS.doc = function(ast, scope, parent)
    assert(utils.root.options.useMetadata, "can't look up doc with metadata disabled.")
    compiler.assert((#ast == 2), "expected one argument", ast)
    local target = utils.deref(ast[2])
    local specialOrMacro = (scope.specials[target] or scope.macros[target])
    if specialOrMacro then
      return ("print([[%s]])"):format(doc_2a(specialOrMacro, target))
    else
      local value = tostring(compiler.compile1(ast[2], scope, parent, {nval = 1})[1])
      return ("print(require('%s').doc(%s, '%s'))"):format((utils.root.options.moduleName or "fennel"), value, tostring(ast[2]))
    end
  end
  docSpecial("doc", {"x"}, "Print the docstring and arglist for a function, macro, or special form.")
  local function dot(ast, scope, parent)
    compiler.assert((1 < #ast), "expected table argument", ast)
    local len = #ast
    local lhs = compiler.compile1(ast[2], scope, parent, {nval = 1})
    if (len == 2) then
      return tostring(lhs[1])
    else
      local indices = {}
      for i = 3, len, 1 do
        local index = ast[i]
        if ((type(index) == "string") and utils.isValidLuaIdentifier(index)) then
          table.insert(indices, ("." .. index))
        else
          index = compiler.compile1(index, scope, parent, {nval = 1})[1]
          table.insert(indices, ("[" .. tostring(index) .. "]"))
        end
      end
      if utils.isTable(ast[2]) then
        return ("(" .. tostring(lhs[1]) .. ")" .. table.concat(indices))
      else
        return (tostring(lhs[1]) .. table.concat(indices))
      end
    end
  end
  SPECIALS["."] = dot
  docSpecial(".", {"tbl", "key1", "..."}, "Look up key1 in tbl table. If more args are provided, do a nested lookup.")
  SPECIALS.global = function(ast, scope, parent)
    compiler.assert((#ast == 3), "expected name and value", ast)
    return compiler.destructure(ast[2], ast[3], ast, scope, parent, {forceglobal = true, nomulti = true})
  end
  docSpecial("global", {"name", "val"}, "Set name as a global with val.")
  SPECIALS.set = function(ast, scope, parent)
    compiler.assert((#ast == 3), "expected name and value", ast)
    return compiler.destructure(ast[2], ast[3], ast, scope, parent, {noundef = true})
  end
  docSpecial("set", {"name", "val"}, "Set a local variable to a new value. Only works on locals using var.")
  local function set_forcibly_21_2a(ast, scope, parent)
    compiler.assert((#ast == 3), "expected name and value", ast)
    return compiler.destructure(ast[2], ast[3], ast, scope, parent, {forceset = true})
  end
  SPECIALS["set-forcibly!"] = set_forcibly_21_2a
  local function local_2a(ast, scope, parent)
    compiler.assert((#ast == 3), "expected name and value", ast)
    return compiler.destructure(ast[2], ast[3], ast, scope, parent, {declaration = true, nomulti = true})
  end
  SPECIALS["local"] = local_2a
  docSpecial("local", {"name", "val"}, "Introduce new top-level immutable local.")
  SPECIALS.var = function(ast, scope, parent)
    compiler.assert((#ast == 3), "expected name and value", ast)
    return compiler.destructure(ast[2], ast[3], ast, scope, parent, {declaration = true, isvar = true, nomulti = true})
  end
  docSpecial("var", {"name", "val"}, "Introduce new mutable local.")
  SPECIALS.let = function(ast, scope, parent, opts)
    local bindings = ast[2]
    local preSyms = {}
    compiler.assert((utils.isList(bindings) or utils.isTable(bindings)), "expected binding table", ast)
    compiler.assert(((#bindings % 2) == 0), "expected even number of name/value bindings", ast[2])
    compiler.assert((#ast >= 3), "expected body expression", ast[1])
    for _ = 1, (opts.nval or 0), 1 do
      table.insert(preSyms, compiler.gensym(scope))
    end
    local subScope = compiler.makeScope(scope)
    local subChunk = {}
    for i = 1, #bindings, 2 do
      compiler.destructure(bindings[i], bindings[(i + 1)], ast, subScope, subChunk, {declaration = true, nomulti = true})
    end
    return SPECIALS["do"](ast, scope, parent, opts, 3, subChunk, subScope, preSyms)
  end
  docSpecial("let", {"[name1 val1 ... nameN valN]", "..."}, "Introduces a new scope in which a given set of local bindings are used.")
  SPECIALS.tset = function(ast, scope, parent)
    compiler.assert((#ast > 3), "expected table, key, and value arguments", ast)
    local root = compiler.compile1(ast[2], scope, parent, {nval = 1})[1]
    local keys = {}
    for i = 3, (#ast - 1), 1 do
      local key = compiler.compile1(ast[i], scope, parent, {nval = 1})[1]
      keys[(#keys + 1)] = tostring(key)
    end
    local value = compiler.compile1(ast[#ast], scope, parent, {nval = 1})[1]
    local rootstr = tostring(root)
    local fmtstr = nil
    if rootstr:match("^{") then
      fmtstr = "do end (%s)[%s] = %s"
    else
      fmtstr = "%s[%s] = %s"
    end
    return compiler.emit(parent, fmtstr:format(tostring(root), table.concat(keys, "]["), tostring(value)), ast)
  end
  docSpecial("tset", {"tbl", "key1", "...", "keyN", "val"}, "Set the value of a table field. Can take additional keys to set\nnested values, but all parents must contain an existing table.")
  local function if_2a(ast, scope, parent, opts)
    local doScope = compiler.makeScope(scope)
    local branches = {}
    local hasElse = ((#ast > 3) and ((#ast % 2) == 0))
    local elseBranch = nil
    local wrapper, innerTail, innerTarget, targetExprs = nil
    if (opts.tail or opts.target or opts.nval) then
      if (opts.nval and (opts.nval ~= 0) and not opts.target) then
        local accum = {}
        targetExprs = {}
        for i = 1, opts.nval, 1 do
          local s = compiler.gensym(scope)
          accum[i] = s
          targetExprs[i] = utils.expr(s, "sym")
        end
        wrapper, innerTail, innerTarget = "target", opts.tail, table.concat(accum, ", ")
      else
        wrapper, innerTail, innerTarget = "none", opts.tail, opts.target
      end
    else
      wrapper, innerTail, innerTarget = "iife", true, nil
    end
    local bodyOpts = {nval = opts.nval, tail = innerTail, target = innerTarget}
    local function compileBody(i)
      local chunk = {}
      local cscope = compiler.makeScope(doScope)
      compiler.keepSideEffects(compiler.compile1(ast[i], cscope, chunk, bodyOpts), chunk, nil, ast[i])
      return {chunk = chunk, scope = cscope}
    end
    for i = 2, (#ast - 1), 2 do
      local condchunk = {}
      local res = compiler.compile1(ast[i], doScope, condchunk, {nval = 1})
      local cond = res[1]
      local branch = compileBody((i + 1))
      branch.cond = cond
      branch.condchunk = condchunk
      branch.nested = ((i ~= 2) and (next(condchunk, nil) == nil))
      table.insert(branches, branch)
    end
    if hasElse then
      elseBranch = compileBody(#ast)
    end
    local s = compiler.gensym(scope)
    local buffer = {}
    local lastBuffer = buffer
    for i = 1, #branches do
      local branch = branches[i]
      local fstr = nil
      if not branch.nested then
        fstr = "if %s then"
      else
        fstr = "elseif %s then"
      end
      local cond = tostring(branch.cond)
      local condLine = nil
      if ((cond == "true") and branch.nested and (i == #branches)) then
        condLine = "else"
      else
        condLine = fstr:format(cond)
      end
      if branch.nested then
        compiler.emit(lastBuffer, branch.condchunk, ast)
      else
        for _, v in ipairs(branch.condchunk) do
          compiler.emit(lastBuffer, v, ast)
        end
      end
      compiler.emit(lastBuffer, condLine, ast)
      compiler.emit(lastBuffer, branch.chunk, ast)
      if (i == #branches) then
        if hasElse then
          compiler.emit(lastBuffer, "else", ast)
          compiler.emit(lastBuffer, elseBranch.chunk, ast)
        elseif (innerTarget and (condLine ~= "else")) then
          compiler.emit(lastBuffer, "else", ast)
          compiler.emit(lastBuffer, ("%s = nil"):format(innerTarget), ast)
        end
        compiler.emit(lastBuffer, "end", ast)
      elseif not branches[(i + 1)].nested then
        local nextBuffer = {}
        compiler.emit(lastBuffer, "else", ast)
        compiler.emit(lastBuffer, nextBuffer, ast)
        compiler.emit(lastBuffer, "end", ast)
        lastBuffer = nextBuffer
      end
    end
    if (wrapper == "iife") then
      local iifeargs = ((scope.vararg and "...") or "")
      compiler.emit(parent, ("local function %s(%s)"):format(tostring(s), iifeargs), ast)
      compiler.emit(parent, buffer, ast)
      compiler.emit(parent, "end", ast)
      return utils.expr(("%s(%s)"):format(tostring(s), iifeargs), "statement")
    elseif (wrapper == "none") then
      for i = 1, #buffer, 1 do
        compiler.emit(parent, buffer[i], ast)
      end
      return {returned = true}
    else
      compiler.emit(parent, ("local %s"):format(innerTarget), ast)
      for i = 1, #buffer, 1 do
        compiler.emit(parent, buffer[i], ast)
      end
      return targetExprs
    end
  end
  SPECIALS["if"] = if_2a
  docSpecial("if", {"cond1", "body1", "...", "condN", "bodyN"}, "Conditional form.\nTakes any number of condition/body pairs and evaluates the first body where\nthe condition evaluates to truthy. Similar to cond in other lisps.")
  SPECIALS.each = function(ast, scope, parent)
    compiler.assert((#ast >= 3), "expected body expression", ast[1])
    local binding = compiler.assert(utils.isTable(ast[2]), "expected binding table", ast)
    local iter = table.remove(binding, #binding)
    local destructures = {}
    local newManglings = {}
    local subScope = compiler.makeScope(scope)
    local function destructureBinding(v)
      if utils.isSym(v) then
        return compiler.declareLocal(v, {}, subScope, ast, newManglings)
      else
        local raw = utils.sym(compiler.gensym(subScope))
        destructures[raw] = v
        return compiler.declareLocal(raw, {}, subScope, ast)
      end
    end
    local bindVars = utils.map(binding, destructureBinding)
    local vals = compiler.compile1(iter, subScope, parent)
    local valNames = utils.map(vals, tostring)
    local chunk = {}
    compiler.emit(parent, ("for %s in %s do"):format(table.concat(bindVars, ", "), table.concat(valNames, ", ")), ast)
    for raw, args in utils.stablepairs(destructures) do
      compiler.destructure(args, raw, ast, subScope, chunk, {declaration = true, nomulti = true})
    end
    compiler.applyManglings(subScope, newManglings, ast)
    compileDo(ast, subScope, chunk, 3)
    compiler.emit(parent, chunk, ast)
    return compiler.emit(parent, "end", ast)
  end
  docSpecial("each", {"[key value (iterator)]", "..."}, "Runs the body once for each set of values provided by the given iterator.\nMost commonly used with ipairs for sequential tables or pairs for  undefined\norder, but can be used with any iterator.")
  local function while_2a(ast, scope, parent)
    local len1 = #parent
    local condition = compiler.compile1(ast[2], scope, parent, {nval = 1})[1]
    local len2 = #parent
    local subChunk = {}
    if (len1 ~= len2) then
      for i = (len1 + 1), len2, 1 do
        subChunk[(#subChunk + 1)] = parent[i]
        parent[i] = nil
      end
      compiler.emit(parent, "while true do", ast)
      compiler.emit(subChunk, ("if not %s then break end"):format(condition[1]), ast)
    else
      compiler.emit(parent, ("while " .. tostring(condition) .. " do"), ast)
    end
    compileDo(ast, compiler.makeScope(scope), subChunk, 3)
    compiler.emit(parent, subChunk, ast)
    return compiler.emit(parent, "end", ast)
  end
  SPECIALS["while"] = while_2a
  docSpecial("while", {"condition", "..."}, "The classic while loop. Evaluates body until a condition is non-truthy.")
  local function for_2a(ast, scope, parent)
    local ranges = compiler.assert(utils.isTable(ast[2]), "expected binding table", ast)
    local bindingSym = table.remove(ast[2], 1)
    local subScope = compiler.makeScope(scope)
    local rangeArgs = {}
    local chunk = {}
    compiler.assert(utils.isSym(bindingSym), ("unable to bind %s %s"):format(type(bindingSym), tostring(bindingSym)), ast[2])
    compiler.assert((#ast >= 3), "expected body expression", ast[1])
    for i = 1, math.min(#ranges, 3), 1 do
      rangeArgs[i] = tostring(compiler.compile1(ranges[i], subScope, parent, {nval = 1})[1])
    end
    compiler.emit(parent, ("for %s = %s do"):format(compiler.declareLocal(bindingSym, {}, subScope, ast), table.concat(rangeArgs, ", ")), ast)
    compileDo(ast, subScope, chunk, 3)
    compiler.emit(parent, chunk, ast)
    return compiler.emit(parent, "end", ast)
  end
  SPECIALS["for"] = for_2a
  docSpecial("for", {"[index start stop step?]", "..."}, "Numeric loop construct.\nEvaluates body once for each value between start and stop (inclusive).")
  local function once(val, ast, scope, parent)
    if ((val.type == "statement") or (val.type == "expression")) then
      local s = compiler.gensym(scope)
      compiler.emit(parent, ("local %s = %s"):format(s, tostring(val)), ast)
      return utils.expr(s, "sym")
    else
      return val
    end
  end
  local function method_call(ast, scope, parent)
    compiler.assert((#ast >= 3), "expected at least 2 arguments", ast)
    local objectexpr = compiler.compile1(ast[2], scope, parent, {nval = 1})[1]
    local methodident, methodstring = false
    if ((type(ast[3]) == "string") and utils.isValidLuaIdentifier(ast[3])) then
      methodident = true
      methodstring = ast[3]
    else
      methodstring = tostring(compiler.compile1(ast[3], scope, parent, {nval = 1})[1])
      objectexpr = once(objectexpr, ast[2], scope, parent)
    end
    local args = {}
    for i = 4, #ast, 1 do
      local subexprs = nil
      local _1_
      if (i ~= #ast) then
        _1_ = 1
      else
        _1_ = nil
      end
      subexprs = compiler.compile1(ast[i], scope, parent, {nval = _1_})
      utils.map(subexprs, tostring, args)
    end
    local fstring = nil
    if not methodident then
      table.insert(args, 1, tostring(objectexpr))
      if (objectexpr.type == "sym") then
        fstring = "%s[%s](%s)"
      else
        fstring = "(%s)[%s](%s)"
      end
    elseif ((objectexpr.type == "literal") or (objectexpr.type == "expression")) then
      fstring = "(%s):%s(%s)"
    else
      fstring = "%s:%s(%s)"
    end
    return utils.expr(fstring:format(tostring(objectexpr), methodstring, table.concat(args, ", ")), "statement")
  end
  SPECIALS[":"] = method_call
  docSpecial(":", {"tbl", "method-name", "..."}, "Call the named method on tbl with the provided args.\nMethod name doesn't have to be known at compile-time; if it is, use\n(tbl:method-name ...) instead.")
  SPECIALS.comment = function(ast, _, parent)
    local els = {}
    for i = 2, #ast, 1 do
      els[(#els + 1)] = tostring(ast[i]):gsub("\n", " ")
    end
    return compiler.emit(parent, ("-- " .. table.concat(els, " ")), ast)
  end
  docSpecial("comment", {"..."}, "Comment which will be emitted in Lua output.")
  SPECIALS.hashfn = function(ast, scope, parent)
    compiler.assert((#ast == 2), "expected one argument", ast)
    local fScope = nil
    do
      local _0_0 = compiler.makeScope(scope)
      _0_0["vararg"] = false
      _0_0["hashfn"] = true
      fScope = _0_0
    end
    local fChunk = {}
    local name = compiler.gensym(scope)
    local symbol = utils.sym(name)
    compiler.declareLocal(symbol, {}, scope, ast)
    local args = {}
    for i = 1, 9 do
      args[i] = compiler.declareLocal(utils.sym(("$" .. i)), {}, fScope, ast)
    end
    local function walker(idx, node, parentNode)
      if (utils.isSym(node) and (utils.deref(node) == "$...")) then
        parentNode[idx] = utils.varg()
        fScope.vararg = true
        return nil
      else
        return (utils.isList(node) or utils.isTable(node))
      end
    end
    utils.walkTree(ast[2], walker)
    compiler.compile1(ast[2], fScope, fChunk, {tail = true})
    local maxUsed = 0
    for i = 1, 9, 1 do
      if fScope.symmeta[("$" .. i)].used then
        maxUsed = i
      end
    end
    if fScope.vararg then
      compiler.assert((maxUsed == 0), "$ and $... in hashfn are mutually exclusive", ast)
      args = {utils.deref(utils.varg())}
      maxUsed = 1
    end
    local argStr = table.concat(args, ", ", 1, maxUsed)
    compiler.emit(parent, ("local function %s(%s)"):format(name, argStr), ast)
    compiler.emit(parent, fChunk, ast)
    compiler.emit(parent, "end", ast)
    return utils.expr(name, "sym")
  end
  docSpecial("hashfn", {"..."}, "Function literal shorthand; args are either $... OR $1, $2, etc.")
  local function defineArithmeticSpecial(name, zeroArity, unaryPrefix, luaName)
    do
      local paddedOp = (" " .. (luaName or name) .. " ")
      local function _0_(ast, scope, parent)
        local len = #ast
        if (len == 1) then
          compiler.assert((zeroArity ~= nil), "Expected more than 0 arguments", ast)
          return utils.expr(zeroArity, "literal")
        else
          local operands = {}
          for i = 2, len, 1 do
            local subexprs = nil
            local _1_
            if (i == 1) then
              _1_ = 1
            else
            _1_ = nil
            end
            subexprs = compiler.compile1(ast[i], scope, parent, {nval = _1_})
            utils.map(subexprs, tostring, operands)
          end
          if (#operands == 1) then
            if unaryPrefix then
              return ("(" .. unaryPrefix .. paddedOp .. operands[1] .. ")")
            else
              return operands[1]
            end
          else
            return ("(" .. table.concat(operands, paddedOp) .. ")")
          end
        end
      end
      SPECIALS[name] = _0_
    end
    return docSpecial(name, {"a", "b", "..."}, "Arithmetic operator; works the same as Lua but accepts more arguments.")
  end
  defineArithmeticSpecial("+", "0")
  defineArithmeticSpecial("..", "''")
  defineArithmeticSpecial("^")
  defineArithmeticSpecial("-", nil, "")
  defineArithmeticSpecial("*", "1")
  defineArithmeticSpecial("%")
  defineArithmeticSpecial("/", nil, "1")
  defineArithmeticSpecial("//", nil, "1")
  defineArithmeticSpecial("lshift", nil, "1", "<<")
  defineArithmeticSpecial("rshift", nil, "1", ">>")
  defineArithmeticSpecial("band", "0", "0", "&")
  defineArithmeticSpecial("bor", "0", "0", "|")
  defineArithmeticSpecial("bxor", "0", "0", "~")
  docSpecial("lshift", {"x", "n"}, "Bitwise logical left shift of x by n bits; only works in Lua 5.3+.")
  docSpecial("rshift", {"x", "n"}, "Bitwise logical right shift of x by n bits; only works in Lua 5.3+.")
  docSpecial("band", {"x1", "x2"}, "Bitwise AND of arguments; only works in Lua 5.3+.")
  docSpecial("bor", {"x1", "x2"}, "Bitwise OR of arguments; only works in Lua 5.3+.")
  docSpecial("bxor", {"x1", "x2"}, "Bitwise XOR of arguments; only works in Lua 5.3+.")
  defineArithmeticSpecial("or", "false")
  defineArithmeticSpecial("and", "true")
  docSpecial("and", {"a", "b", "..."}, "Boolean operator; works the same as Lua but accepts more arguments.")
  docSpecial("or", {"a", "b", "..."}, "Boolean operator; works the same as Lua but accepts more arguments.")
  docSpecial("..", {"a", "b", "..."}, "String concatenation operator; works the same as Lua but accepts more arguments.")
  local function defineComparatorSpecial(name, realop, chainOp)
    do
      local op = (realop or name)
      local function opfn(ast, scope, parent)
        local len = #ast
        compiler.assert((len > 2), "expected at least two arguments", ast)
        local lhs = compiler.compile1(ast[2], scope, parent, {nval = 1})[1]
        local lastval = compiler.compile1(ast[3], scope, parent, {nval = 1})[1]
        if (len > 3) then
          lastval = once(lastval, ast[3], scope, parent)
        end
        local out = ("(%s %s %s)"):format(tostring(lhs), op, tostring(lastval))
        if (len > 3) then
          for i = 4, len do
            local nextval = once(compiler.compile1(ast[i], scope, parent, {nval = 1})[1], ast[i], scope, parent)
            out = ((out .. " %s (%s %s %s)")):format((chainOp or "and"), tostring(lastval), op, tostring(nextval))
            lastval = nextval
          end
          out = ("(" .. out .. ")")
        end
        return out
      end
      SPECIALS[name] = opfn
    end
    return docSpecial(name, {"a", "b", "..."}, "Comparison operator; works the same as Lua but accepts more arguments.")
  end
  defineComparatorSpecial(">")
  defineComparatorSpecial("<")
  defineComparatorSpecial(">=")
  defineComparatorSpecial("<=")
  defineComparatorSpecial("=", "==")
  defineComparatorSpecial("not=", "~=", "or")
  SPECIALS["~="] = SPECIALS["not="]
  local function defineUnarySpecial(op, realop)
    local function opfn(ast, scope, parent)
      compiler.assert((#ast == 2), "expected one argument", ast)
      local tail = compiler.compile1(ast[2], scope, parent, {nval = 1})
      return ((realop or op) .. tostring(tail[1]))
    end
    SPECIALS[op] = opfn
    return nil
  end
  defineUnarySpecial("not", "not ")
  docSpecial("not", {"x"}, "Logical operator; works the same as Lua.")
  defineUnarySpecial("bnot", "~")
  docSpecial("bnot", {"x"}, "Bitwise negation; only works in Lua 5.3+.")
  defineUnarySpecial("length", "#")
  docSpecial("length", {"x"}, "Returns the length of a table or string.")
  SPECIALS["#"] = SPECIALS.length
  SPECIALS.quote = function(ast, scope, parent)
    compiler.assert((#ast == 2), "expected one argument")
    local runtime, thisScope = true, scope
    while thisScope do
      thisScope = thisScope.parent
      if (thisScope == compiler.scopes.compiler) then
        runtime = false
      end
    end
    return compiler.doQuote(ast[2], scope, parent, runtime)
  end
  docSpecial("quote", {"x"}, "Quasiquote the following form. Only works in macro/compiler scope.")
  local function makeCompilerEnv(ast, scope, parent)
    local function _0_()
      return compiler.scopes.macro
    end
    local function _1_(symbol)
      compiler.assert(compiler.scopes.macro, "must call from macro", ast)
      return compiler.scopes.macro.manglings[tostring(symbol)]
    end
    local function _2_()
      return utils.sym(compiler.gensym((compiler.scopes.macro or scope)))
    end
    local function _3_(form)
      compiler.assert(compiler.scopes.macro, "must call from macro", ast)
      return compiler.macroexpand(form, compiler.scopes.macro)
    end
    return setmetatable({["get-scope"] = _0_, ["in-scope?"] = _1_, ["list?"] = utils.isList, ["multi-sym?"] = utils.isMultiSym, ["sequence?"] = utils.isSequence, ["sym?"] = utils.isSym, ["table?"] = utils.isTable, ["varg?"] = utils.isVarg, _AST = ast, _CHUNK = parent, _IS_COMPILER = true, _SCOPE = scope, _SPECIALS = compiler.scopes.global.specials, _VARARG = utils.varg(), fennel = utils.fennelModule, gensym = _2_, list = utils.list, macroexpand = _3_, sequence = utils.sequence, sym = utils.sym, unpack = unpack}, {__index = (_ENV or _G)})
  end
  local cfg = string.gmatch(package.config, "([^\n]+)")
  local dirsep, pathsep, pathmark = (cfg() or "/"), (cfg() or ";"), (cfg() or "?")
  local pkgConfig = {dirsep = dirsep, pathmark = pathmark, pathsep = pathsep}
  local function escapepat(str)
    return string.gsub(str, "[^%w]", "%%%1")
  end
  local function searchModule(modulename, pathstring)
    local pathsepesc = escapepat(pkgConfig.pathsep)
    local pattern = ("([^%s]*)%s"):format(pathsepesc, pathsepesc)
    local nodotModule = modulename:gsub("%.", pkgConfig.dirsep)
    local fullpath = ((pathstring or utils.fennelModule.path) .. pkgConfig.pathsep)
    local function try_path(path)
      local filename = path:gsub(escapepat(pkgConfig.pathmark), nodotModule)
      local filename2 = path:gsub(escapepat(pkgConfig.pathmark), modulename)
      local _0_0 = (io.open(filename) or io.open(filename2))
      if (nil ~= _0_0) then
        local file = _0_0
        file:close()
        return filename
      end
    end
    local function find_in_path(start)
      local _0_0 = fullpath:match(pattern, start)
      if (nil ~= _0_0) then
        local path = _0_0
        return (try_path(path) or find_in_path((start + #path + 1)))
      end
    end
    return find_in_path(1)
  end
  local function makeSearcher(options)
    local function _0_(module_name)
      local opts = utils.copy(utils.rootOptions)
      local filename = searchModule(module_name)
      for k, v in pairs((options or {})) do
        opts[k] = v
      end
      if filename then
        local function _1_(mod_name)
          return utils.fennelModule.dofile(filename, opts, mod_name)
        end
        return _1_
      end
    end
    return _0_
  end
  local function macroGlobals(env, globals)
    local allowed = currentGlobalNames(env)
    for _, k in pairs((globals or {})) do
      table.insert(allowed, k)
    end
    return allowed
  end
  local function addMacros(macros_2a, ast, scope)
    compiler.assert(utils.isTable(macros_2a), "expected macros to be table", ast)
    for k, v in pairs(macros_2a) do
      compiler.assert((type(v) == "function"), "expected each macro to be function", ast)
      scope.macros[k] = v
    end
    return nil
  end
  local function loadMacros(modname, ast, scope, parent)
    local filename = compiler.assert(searchModule(modname), (modname .. " module not found."), ast)
    local env = makeCompilerEnv(ast, scope, parent)
    local globals = macroGlobals(env, currentGlobalNames())
    return utils.fennelModule.dofile(filename, {allowedGlobals = globals, env = env, scope = compiler.scopes.compiler, useMetadata = utils.root.options.useMetadata})
  end
  local macroLoaded = {}
  SPECIALS["require-macros"] = function(ast, scope, parent)
    compiler.assert((#ast == 2), "Expected one module name argument", ast)
    local modname = ast[2]
    if not macroLoaded[modname] then
      macroLoaded[modname] = loadMacros(modname, ast, scope, parent)
    end
    return addMacros(macroLoaded[modname], ast, scope, parent)
  end
  docSpecial("require-macros", {"macro-module-name"}, "Load given module and use its contents as macro definitions in current scope.\nMacro module should return a table of macro functions with string keys.\nConsider using import-macros instead as it is more flexible.")
  local function emit_fennel(src, path, opts, sub_chunk)
    local subscope = compiler.makeScope(utils.root.scope.parent)
    local forms = {}
    if utils.root.options.requireAsInclude then
      subscope.specials.require = compiler.requireInclude
    end
    for _, val in parser.parser(parser.stringStream(src), path) do
      table.insert(forms, val)
    end
    for i = 1, #forms do
      local subopts = nil
      if (i == #forms) then
        subopts = {nval = 1, tail = true}
      else
        subopts = {nval = 0}
      end
      utils.propagateOptions(opts, subopts)
      compiler.compile1(forms[i], subscope, sub_chunk, subopts)
    end
    return nil
  end
  local function include_path(ast, opts, path, mod, fennel_3f)
    utils.root.scope.includes[mod] = "fnl/loading"
    local src = nil
    do
      local f = assert(io.open(path))
      local function close_handlers_0_(ok_0_, ...)
        f:close()
        if ok_0_ then
          return ...
        else
          return error(..., 0)
        end
      end
      local function _0_()
        return f:read("*all"):gsub("[\13\n]*$", "")
      end
      src = close_handlers_0_(xpcall(_0_, (package.loaded.fennel or debug).traceback))
    end
    local ret = utils.expr(("require(\"" .. mod .. "\")"), "statement")
    local target = ("package.preload[%q]"):format(mod)
    local preload_str = (target .. " = " .. target .. " or function(...)")
    local temp_chunk, sub_chunk = {}, {}
    compiler.emit(temp_chunk, preload_str, ast)
    compiler.emit(temp_chunk, sub_chunk)
    compiler.emit(temp_chunk, "end", ast)
    for i, v in ipairs(temp_chunk) do
      table.insert(utils.root.chunk, i, v)
    end
    if fennel_3f then
      emit_fennel(src, path, opts, sub_chunk)
    else
      compiler.emit(sub_chunk, src, ast)
    end
    utils.root.scope.includes[mod] = ret
    return ret
  end
  local function include_circular_fallback(mod, modexpr, fallback, ast)
    if (utils.root.scope.includes[mod] == "fnl/loading") then
      compiler.assert(fallback, "circular include detected", ast)
      return fallback(modexpr)
    end
  end
  SPECIALS.include = function(ast, scope, parent, opts)
    compiler.assert((#ast == 2), "expected one argument", ast)
    local modexpr = compiler.compile1(ast[2], scope, parent, {nval = 1})[1]
    if ((modexpr.type ~= "literal") or ((modexpr[1]):byte() ~= 34)) then
      if opts.fallback then
        return opts.fallback(modexpr)
      else
        return compiler.assert(false, "module name must be string literal", ast)
      end
    else
      local mod = loadCode(("return " .. modexpr[1]))()
      local function _1_()
        local _0_0 = searchModule(mod)
        if (nil ~= _0_0) then
          local fennel_path = _0_0
          return include_path(ast, opts, fennel_path, mod, true)
        else
          local _ = _0_0
          local lua_path = searchModule(mod, package.path)
          if lua_path then
            return include_path(ast, opts, lua_path, mod, false)
          elseif opts.fallback then
            return opts.fallback(modexpr)
          else
            return compiler.assert(false, ("module not found " .. mod), ast)
          end
        end
      end
      return (include_circular_fallback(mod, modexpr, opts.fallback, ast) or utils.root.scope.includes[mod] or _1_())
    end
  end
  docSpecial("include", {"module-name-literal"}, "Like require but load the target module during compilation and embed it in the\nLua output. The module must be a string literal and resolvable at compile time.")
  local function evalCompiler(ast, scope, parent)
    local scope0 = compiler.makeScope(compiler.scopes.compiler)
    local luasrc = compiler.compile(ast, {scope = scope0, useMetadata = utils.root.options.useMetadata})
    local loader = loadCode(luasrc, wrapEnv(makeCompilerEnv(ast, scope0, parent)))
    return loader()
  end
  SPECIALS.macros = function(ast, scope, parent)
    compiler.assert((#ast == 2), "Expected one table argument", ast)
    return addMacros(evalCompiler(ast[2], scope, parent), ast, scope, parent)
  end
  docSpecial("macros", {"{:macro-name-1 (fn [...] ...) ... :macro-name-N macro-body-N}"}, "Define all functions in the given table as macros local to the current scope.")
  SPECIALS["eval-compiler"] = function(ast, scope, parent)
    local oldFirst = ast[1]
    ast[1] = utils.sym("do")
    local val = evalCompiler(ast, scope, parent)
    ast[1] = oldFirst
    return val
  end
  docSpecial("eval-compiler", {"..."}, "Evaluate the body at compile-time. Use the macro system instead if possible.")
  return {currentGlobalNames = currentGlobalNames, doc = doc_2a, loadCode = loadCode, macroLoaded = macroLoaded, makeCompilerEnv = makeCompilerEnv, makeSearcher = makeSearcher, searchModule = searchModule, wrapEnv = wrapEnv}
end
package.preload["fennel.compiler"] = package.preload["fennel.compiler"] or function(...)
  local utils = require("fennel.utils")
  local parser = require("fennel.parser")
  local friend = require("fennel.friend")
  local unpack = (_G.unpack or table.unpack)
  local scopes = {}
  local function makeScope(parent)
    local parent0 = (parent or scopes.global)
    local _0_
    if parent0 then
      _0_ = ((parent0.depth or 0) + 1)
    else
      _0_ = 0
    end
    return {autogensyms = {}, depth = _0_, hashfn = (parent0 and parent0.hashfn), includes = setmetatable({}, {__index = (parent0 and parent0.includes)}), macros = setmetatable({}, {__index = (parent0 and parent0.macros)}), manglings = setmetatable({}, {__index = (parent0 and parent0.manglings)}), parent = parent0, refedglobals = setmetatable({}, {__index = (parent0 and parent0.refedglobals)}), specials = setmetatable({}, {__index = (parent0 and parent0.specials)}), symmeta = setmetatable({}, {__index = (parent0 and parent0.symmeta)}), unmanglings = setmetatable({}, {__index = (parent0 and parent0.unmanglings)}), vararg = (parent0 and parent0.vararg)}
  end
  local function assertCompile(condition, msg, ast)
    if not condition then
      local _0_ = (utils.root.options or {})
      local source = _0_["source"]
      local unfriendly = _0_["unfriendly"]
      utils.root.reset()
      if unfriendly then
        local m = getmetatable(ast)
        local filename = ((m and m.filename) or ast.filename or "unknown")
        local line = ((m and m.line) or ast.line or "?")
        local target = nil
        local function _1_()
          if utils.isSym(ast[1]) then
            return utils.deref(ast[1])
          else
            return (ast[1] or "()")
          end
        end
        target = tostring(_1_())
        error(string.format("Compile error in '%s' %s:%s: %s", target, filename, line, msg), 0)
      else
        friend["assert-compile"](condition, msg, ast, source)
      end
    end
    return condition
  end
  scopes.global = makeScope()
  scopes.global.vararg = true
  scopes.compiler = makeScope(scopes.global)
  scopes.macro = scopes.global
  local serializeSubst = {["\11"] = "\\v", ["\12"] = "\\f", ["\7"] = "\\a", ["\8"] = "\\b", ["\9"] = "\\t", ["\n"] = "n"}
  local function serializeString(str)
    local function _0_(_241)
      return ("\\" .. _241:byte())
    end
    return ("%q"):format(str):gsub(".", serializeSubst):gsub("[\128-\255]", _0_)
  end
  local function globalMangling(str)
    if utils.isValidLuaIdentifier(str) then
      return str
    else
      local function _0_(_241)
        return ("_%02x"):format(_241:byte())
      end
      return ("__fnl_global__" .. str:gsub("[^%w]", _0_))
    end
  end
  local function globalUnmangling(identifier)
    local _0_0 = identifier:match("^__fnl_global__(.*)$")
    if (nil ~= _0_0) then
      local rest = _0_0
      local _1_0 = nil
      local function _2_(_241)
        return string.char(tonumber(_241:sub(2), 16))
      end
      _1_0 = rest:gsub("_[%da-f][%da-f]", _2_)
      return _1_0
    else
      local _ = _0_0
      return identifier
    end
  end
  local allowedGlobals = nil
  local function globalAllowed(name)
    local found_3f = not allowedGlobals
    if not allowedGlobals then
      return true
    else
      for _, g in ipairs(allowedGlobals) do
        if (g == name) then
          found_3f = true
        end
      end
      return found_3f
    end
  end
  local function localMangling(str, scope, ast, tempManglings)
    local append = 0
    local mangling = str
    assertCompile(not utils.isMultiSym(str), ("unexpected multi symbol " .. str), ast)
    if (utils.luaKeywords[mangling] or mangling:match("^%d")) then
      mangling = ("_" .. mangling)
    end
    local function _1_(_241)
      return ("_%02x"):format(_241:byte())
    end
    mangling = mangling:gsub("-", "_"):gsub("[^%w_]", _1_)
    local raw = mangling
    while scope.unmanglings[mangling] do
      mangling = (raw .. append)
      append = (append + 1)
    end
    scope.unmanglings[mangling] = str
    do
      local manglings = (tempManglings or scope.manglings)
      manglings[str] = mangling
    end
    return mangling
  end
  local function applyManglings(scope, newManglings, ast)
    for raw, mangled in pairs(newManglings) do
      assertCompile(not scope.refedglobals[mangled], ("use of global " .. raw .. " is aliased by a local"), ast)
      scope.manglings[raw] = mangled
    end
    return nil
  end
  local function combineParts(parts, scope)
    local ret = (scope.manglings[parts[1]] or globalMangling(parts[1]))
    for i = 2, #parts, 1 do
      if utils.isValidLuaIdentifier(parts[i]) then
        if (parts.multiSymMethodCall and (i == #parts)) then
          ret = (ret .. ":" .. parts[i])
        else
          ret = (ret .. "." .. parts[i])
        end
      else
        ret = (ret .. "[" .. serializeString(parts[i]) .. "]")
      end
    end
    return ret
  end
  local function gensym(scope, base)
    local append, mangling = 0, ((base or "") .. "_0_")
    while scope.unmanglings[mangling] do
      mangling = ((base or "") .. "_" .. append .. "_")
      append = (append + 1)
    end
    scope.unmanglings[mangling] = true
    return mangling
  end
  local function autogensym(base, scope)
    local _0_0 = utils.isMultiSym(base)
    if (nil ~= _0_0) then
      local parts = _0_0
      parts[1] = autogensym(parts[1], scope)
      return table.concat(parts, ((parts.multiSymMethodCall and ":") or "."))
    else
      local _ = _0_0
      local function _1_()
        local mangling = gensym(scope, base:sub(1, ( - 2)))
        scope.autogensyms[base] = mangling
        return mangling
      end
      return (scope.autogensyms[base] or _1_())
    end
  end
  local function checkBindingValid(symbol, scope, ast)
    local name = utils.deref(symbol)
    assertCompile(not (scope.specials[name] or scope.macros[name]), ("local %s was overshadowed by a special form or macro"):format(name), ast)
    return assertCompile(not utils.isQuoted(symbol), ("macro tried to bind %s without gensym"):format(name), symbol)
  end
  local function declareLocal(symbol, meta, scope, ast, tempManglings)
    checkBindingValid(symbol, scope, ast)
    local name = utils.deref(symbol)
    assertCompile(not utils.isMultiSym(name), ("unexpected multi symbol " .. name), ast)
    scope.symmeta[name] = meta
    return localMangling(name, scope, ast, tempManglings)
  end
  local function symbolToExpression(symbol, scope, isReference)
    local name = symbol[1]
    local multiSymParts = utils.isMultiSym(name)
    if scope.hashfn then
      if (name == "$") then
        name = "$1"
      end
      if multiSymParts then
        if (multiSymParts[1] == "$") then
          multiSymParts[1] = "$1"
          name = table.concat(multiSymParts, ".")
        end
      end
    end
    local parts = (multiSymParts or {name})
    local etype = (((#parts > 1) and "expression") or "sym")
    local isLocal = scope.manglings[parts[1]]
    if (isLocal and scope.symmeta[parts[1]]) then
      scope.symmeta[parts[1]]["used"] = true
    end
    assertCompile((not isReference or isLocal or globalAllowed(parts[1])), ("unknown global in strict mode: " .. parts[1]), symbol)
    if (allowedGlobals and not isLocal) then
      utils.root.scope.refedglobals[parts[1]] = true
    end
    return utils.expr(combineParts(parts, scope), etype)
  end
  local function emit(chunk, out, ast)
    if (type(out) == "table") then
      return table.insert(chunk, out)
    else
      return table.insert(chunk, {ast = ast, leaf = out})
    end
  end
  local function peephole(chunk)
    if chunk.leaf then
      return chunk
    elseif ((#chunk >= 3) and (chunk[(#chunk - 2)].leaf == "do") and not chunk[(#chunk - 1)].leaf and (chunk[#chunk].leaf == "end")) then
      local kid = peephole(chunk[(#chunk - 1)])
      local newChunk = {ast = chunk.ast}
      for i = 1, (#chunk - 3), 1 do
        table.insert(newChunk, peephole(chunk[i]))
      end
      for i = 1, #kid, 1 do
        table.insert(newChunk, kid[i])
      end
      return newChunk
    else
      return utils.map(chunk, peephole)
    end
  end
  local function flattenChunkCorrelated(mainChunk)
    local function flatten(chunk, out, lastLine, file)
      local last_line = lastLine
      if chunk.leaf then
        out[last_line] = ((out[last_line] or "") .. " " .. chunk.leaf)
      else
        for _, subchunk in ipairs(chunk) do
          if (subchunk.leaf or (#subchunk > 0)) then
            if (subchunk.ast and (file == subchunk.ast.file)) then
              last_line = math.max(last_line, (subchunk.ast.line or 0))
            end
            last_line = flatten(subchunk, out, last_line, file)
          end
        end
      end
      return last_line
    end
    local out = {}
    local last = flatten(mainChunk, out, 1, mainChunk.file)
    for i = 1, last do
      if (out[i] == nil) then
        out[i] = ""
      end
    end
    return table.concat(out, "\n")
  end
  local function flattenChunk(sm, chunk, tab, depth)
    if chunk.leaf then
      local code = chunk.leaf
      local info = chunk.ast
      if sm then
        sm[(#sm + 1)] = ((info and info.line) or ( - 1))
      end
      return code
    else
      local tab0 = nil
      do
        local _0_0 = tab
        if (_0_0 == true) then
          tab0 = "  "
        elseif (_0_0 == false) then
          tab0 = ""
        elseif (_0_0 == tab) then
          tab0 = tab
        elseif (_0_0 == nil) then
          tab0 = ""
        else
        tab0 = nil
        end
      end
      local function parter(c)
        if (c.leaf or (#c > 0)) then
          local sub = flattenChunk(sm, c, tab0, (depth + 1))
          if (depth > 0) then
            sub = (tab0 .. sub:gsub("\n", ("\n" .. tab0)))
          end
          return sub
        end
      end
      return table.concat(utils.map(chunk, parter), "\n")
    end
  end
  local fennelSourcemap = {}
  local function makeShortSrc(source)
    local source0 = source:gsub("\n", " ")
    if (#source0 <= 49) then
      return ("[fennel \"" .. source0 .. "\"]")
    else
      return ("[fennel \"" .. source0:sub(1, 46) .. "...\"]")
    end
  end
  local function flatten(chunk, options)
    local chunk0 = peephole(chunk)
    if options.correlate then
      return flattenChunkCorrelated(chunk0), {}
    else
      local sm = {}
      local ret = flattenChunk(sm, chunk0, options.indent, 0)
      if sm then
        local key, short_src = nil
        if options.filename then
          short_src = options.filename
          key = ("@" .. short_src)
        else
          key = ret
          short_src = makeShortSrc((options.source or ret))
        end
        sm.short_src = short_src
        sm.key = key
        fennelSourcemap[key] = sm
      end
      return ret, sm
    end
  end
  local function makeMetadata()
    local function _0_(self, tgt, key)
      if self[tgt] then
        return self[tgt][key]
      end
    end
    local function _1_(self, tgt, key, value)
      self[tgt] = (self[tgt] or {})
      self[tgt][key] = value
      return tgt
    end
    local function _2_(self, tgt, ...)
      local kvLen = select("#", ...)
      local kvs = {...}
      if ((kvLen % 2) ~= 0) then
        error("metadata:setall() expected even number of k/v pairs")
      end
      self[tgt] = (self[tgt] or {})
      for i = 1, kvLen, 2 do
        self[tgt][kvs[i]] = kvs[(i + 1)]
      end
      return tgt
    end
    return setmetatable({}, {__index = {get = _0_, set = _1_, setall = _2_}, __mode = "k"})
  end
  local function exprs1(exprs)
    return table.concat(utils.map(exprs, 1), ", ")
  end
  local function keepSideEffects(exprs, chunk, start, ast)
    local start0 = (start or 1)
    for j = start0, #exprs, 1 do
      local se = exprs[j]
      if ((se.type == "expression") and (se[1] ~= "nil")) then
        emit(chunk, ("do local _ = %s end"):format(tostring(se)), ast)
      elseif (se.type == "statement") then
        local code = tostring(se)
        emit(chunk, (((code:byte() == 40) and ("do end " .. code)) or code), ast)
      end
    end
    return nil
  end
  local function handleCompileOpts(exprs, parent, opts, ast)
    if opts.nval then
      local n = opts.nval
      local len = #exprs
      if (n ~= len) then
        if (len > n) then
          keepSideEffects(exprs, parent, (n + 1), ast)
          for i = (n + 1), len, 1 do
            exprs[i] = nil
          end
        else
          for i = (#exprs + 1), n, 1 do
            exprs[i] = utils.expr("nil", "literal")
          end
        end
      end
    end
    if opts.tail then
      emit(parent, ("return %s"):format(exprs1(exprs)), ast)
    end
    if opts.target then
      local result = exprs1(exprs)
      if (result == "") then
        result = "nil"
      end
      emit(parent, ("%s = %s"):format(opts.target, result), ast)
    end
    if (opts.tail or opts.target) then
      return {}
    else
      return exprs
    end
  end
  local function macroexpand_2a(ast, scope, once)
    if not utils.isList(ast) then
      return ast
    else
      local multiSymParts = utils.isMultiSym(ast[1])
      local macro_2a = (utils.isSym(ast[1]) and scope.macros[utils.deref(ast[1])])
      if (not macro_2a and multiSymParts) then
        local inMacroModule = nil
        macro_2a = scope.macros
        for i = 1, #multiSymParts, 1 do
          macro_2a = (utils.isTable(macro_2a) and macro_2a[multiSymParts[i]])
          if macro_2a then
            inMacroModule = true
          end
        end
        assertCompile((not inMacroModule or (type(macro_2a) == "function")), "macro not found in imported macro module", ast)
      end
      if not macro_2a then
        return ast
      else
        local oldScope = scopes.macro
        local _ = nil
        scopes.macro = scope
        _ = nil
        local ok, transformed = pcall(macro_2a, unpack(ast, 2))
        scopes.macro = oldScope
        assertCompile(ok, transformed, ast)
        if (once or not transformed) then
          return transformed
        else
          return macroexpand_2a(transformed, scope)
        end
      end
    end
  end
  local function compile1(ast, scope, parent, opts)
    local opts0 = (opts or {})
    local ast0 = macroexpand_2a(ast, scope)
    local exprs = {}
    if utils.isList(ast0) then
      local len = #ast0
      local first = ast0[1]
      local multiSymParts = utils.isMultiSym(first)
      local special = (utils.isSym(first) and scope.specials[utils.deref(first)])
      assertCompile((#ast0 > 0), "expected a function, macro, or special to call", ast0)
      if special then
        exprs = (special(ast0, scope, parent, opts0) or utils.expr("nil", "literal"))
        if (type(exprs) == "string") then
          exprs = utils.expr(exprs, "expression")
        end
        if utils.isExpr(exprs) then
          exprs = {exprs}
        end
        if not exprs.returned then
          exprs = handleCompileOpts(exprs, parent, opts0, ast0)
        elseif (opts0.tail or opts0.target) then
          exprs = {}
        end
      elseif (multiSymParts and multiSymParts.multiSymMethodCall) then
        local tableWithMethod = table.concat({unpack(multiSymParts, 1, (#multiSymParts - 1))}, ".")
        local methodToCall = multiSymParts[#multiSymParts]
        local newAST = utils.list(utils.sym(":", scope), utils.sym(tableWithMethod, scope), methodToCall)
        for i = 2, len, 1 do
          newAST[(#newAST + 1)] = ast0[i]
        end
        exprs = compile1(newAST, scope, parent, opts0)
      else
        local fargs = {}
        local fcallee = compile1(ast0[1], scope, parent, {nval = 1})[1]
        assertCompile((fcallee.type ~= "literal"), ("cannot call literal value " .. tostring(ast0[1])), ast0)
        fcallee = tostring(fcallee)
        for i = 2, len, 1 do
          local subexprs = compile1(ast0[i], scope, parent, {nval = (((i ~= len) and 1) or nil)})
          fargs[(#fargs + 1)] = (subexprs[1] or utils.expr("nil", "literal"))
          if (i == len) then
            for j = 2, #subexprs, 1 do
              fargs[(#fargs + 1)] = subexprs[j]
            end
          else
            keepSideEffects(subexprs, parent, 2, ast0[i])
          end
        end
        local call = ("%s(%s)"):format(tostring(fcallee), exprs1(fargs))
        exprs = handleCompileOpts({utils.expr(call, "statement")}, parent, opts0, ast0)
      end
    elseif utils.isVarg(ast0) then
      assertCompile(scope.vararg, "unexpected vararg", ast0)
      exprs = handleCompileOpts({utils.expr("...", "varg")}, parent, opts0, ast0)
    elseif utils.isSym(ast0) then
      local multiSymParts = utils.isMultiSym(ast0)
      local e = nil
      assertCompile(not (multiSymParts and multiSymParts.multiSymMethodCall), "multisym method calls may only be in call position", ast0)
      if (ast0[1] == "nil") then
        e = utils.expr("nil", "literal")
      else
        e = symbolToExpression(ast0, scope, true)
      end
      exprs = handleCompileOpts({e}, parent, opts0, ast0)
    elseif ((type(ast0) == "nil") or (type(ast0) == "boolean")) then
      exprs = handleCompileOpts({utils.expr(tostring(ast0), "literal")}, parent, opts0)
    elseif (type(ast0) == "number") then
      local n = ("%.17g"):format(ast0)
      exprs = handleCompileOpts({utils.expr(n, "literal")}, parent, opts0)
    elseif (type(ast0) == "string") then
      local s = serializeString(ast0)
      exprs = handleCompileOpts({utils.expr(s, "literal")}, parent, opts0)
    elseif (type(ast0) == "table") then
      local buffer = {}
      for i = 1, #ast0, 1 do
        local nval = ((i ~= #ast0) and 1)
        buffer[(#buffer + 1)] = exprs1(compile1(ast0[i], scope, parent, {nval = nval}))
      end
      local function writeOtherValues(k)
        if ((type(k) ~= "number") or (math.floor(k) ~= k) or (k < 1) or (k > #ast0)) then
          if ((type(k) == "string") and utils.isValidLuaIdentifier(k)) then
            return {k, k}
          else
            local _0_ = compile1(k, scope, parent, {nval = 1})
            local compiled = _0_[1]
            local kstr = ("[" .. tostring(compiled) .. "]")
            return {kstr, k}
          end
        end
      end
      do
        local keys = nil
        do
          local _0_0 = utils.kvmap(ast0, writeOtherValues)
          local function _1_(a, b)
            return (a[1] < b[1])
          end
          table.sort(_0_0, _1_)
          keys = _0_0
        end
        local function _1_(k)
          local v = tostring(compile1(ast0[k[2]], scope, parent, {nval = 1})[1])
          return ("%s = %s"):format(k[1], v)
        end
        utils.map(keys, _1_, buffer)
      end
      exprs = handleCompileOpts({utils.expr(("{" .. table.concat(buffer, ", ") .. "}"), "expression")}, parent, opts0, ast0)
    else
      assertCompile(false, ("could not compile value of type " .. type(ast0)), ast0)
    end
    exprs.returned = true
    return exprs
  end
  local function destructure(to, from, ast, scope, parent, opts)
    local opts0 = (opts or {})
    local _0_ = opts0
    local declaration = _0_["declaration"]
    local forceglobal = _0_["forceglobal"]
    local forceset = _0_["forceset"]
    local isvar = _0_["isvar"]
    local nomulti = _0_["nomulti"]
    local noundef = _0_["noundef"]
    local setter = nil
    if declaration then
      setter = "local %s = %s"
    else
      setter = "%s = %s"
    end
    local newManglings = {}
    local function getname(symbol, up1)
      local raw = symbol[1]
      assertCompile(not (nomulti and utils.isMultiSym(raw)), ("unexpected multi symbol " .. raw), up1)
      if declaration then
        return declareLocal(symbol, {var = isvar}, scope, symbol, newManglings)
      else
        local parts = (utils.isMultiSym(raw) or {raw})
        local meta = scope.symmeta[parts[1]]
        if ((#parts == 1) and not forceset) then
          assertCompile(not (forceglobal and meta), ("global %s conflicts with local"):format(tostring(symbol)), symbol)
          assertCompile(not (meta and not meta.var), ("expected var " .. raw), symbol)
          assertCompile((meta or not noundef), ("expected local " .. parts[1]), symbol)
        end
        if forceglobal then
          assertCompile(not scope.symmeta[scope.unmanglings[raw]], ("global " .. raw .. " conflicts with local"), symbol)
          scope.manglings[raw] = globalMangling(raw)
          scope.unmanglings[globalMangling(raw)] = raw
          if allowedGlobals then
            table.insert(allowedGlobals, raw)
          end
        end
        return symbolToExpression(symbol, scope)[1]
      end
    end
    local function compileTopTarget(lvalues)
      local inits = nil
      local function _2_(_241)
        if scope.manglings[_241] then
          return _241
        else
          return "nil"
        end
      end
      inits = utils.map(lvalues, _2_)
      local init = table.concat(inits, ", ")
      local lvalue = table.concat(lvalues, ", ")
      local plen, plast = #parent, parent[#parent]
      local ret = compile1(from, scope, parent, {target = lvalue})
      if declaration then
        for pi = plen, #parent do
          if (parent[pi] == plast) then
            plen = pi
          end
        end
        if ((#parent == (plen + 1)) and parent[#parent].leaf) then
          parent[#parent]["leaf"] = ("local " .. parent[#parent].leaf)
        else
          table.insert(parent, (plen + 1), {ast = ast, leaf = ("local " .. lvalue .. " = " .. init)})
        end
      end
      return ret
    end
    local function destructure1(left, rightexprs, up1, top)
      if (utils.isSym(left) and (left[1] ~= "nil")) then
        local lname = getname(left, up1)
        checkBindingValid(left, scope, left)
        if top then
          compileTopTarget({lname})
        else
          emit(parent, setter:format(lname, exprs1(rightexprs)), left)
        end
      elseif utils.isTable(left) then
        local s = gensym(scope)
        local right = nil
        if top then
          right = exprs1(compile1(from, scope, parent))
        else
          right = exprs1(rightexprs)
        end
        if (right == "") then
          right = "nil"
        end
        emit(parent, ("local %s = %s"):format(s, right), left)
        for k, v in utils.stablepairs(left) do
          if (utils.isSym(left[k]) and (left[k][1] == "&")) then
            assertCompile(((type(k) == "number") and not left[(k + 2)]), "expected rest argument before last parameter", left)
            local formatted = ("{(table.unpack or unpack)(%s, %s)}"):format(s, k)
            local subexpr = utils.expr(formatted, "expression")
            destructure1(left[(k + 1)], {subexpr}, left)
            return
          else
            if (utils.isSym(k) and (tostring(k) == ":") and utils.isSym(v)) then
              k = tostring(v)
            end
            if (type(k) ~= "number") then
              k = serializeString(k)
            end
            local subexpr = utils.expr(("%s[%s]"):format(s, k), "expression")
            destructure1(v, {subexpr}, left)
          end
        end
      elseif utils.isList(left) then
        local leftNames, tables = {}, {}
        for i, name in ipairs(left) do
          local symname = nil
          if utils.isSym(name) then
            symname = getname(name, up1)
          else
            symname = gensym(scope)
            tables[i] = {name, utils.expr(symname, "sym")}
          end
          table.insert(leftNames, symname)
        end
        if top then
          compileTopTarget(leftNames)
        else
          local lvalue = table.concat(leftNames, ", ")
          local setting = setter:format(lvalue, exprs1(rightexprs))
          emit(parent, setting, left)
        end
        for _, pair in utils.stablepairs(tables) do
          destructure1(pair[1], {pair[2]}, left)
        end
      else
        assertCompile(false, ("unable to bind %s %s"):format(type(left), tostring(left)), (((type(up1[2]) == "table") and up1[2]) or up1))
      end
      if top then
        return {returned = true}
      end
    end
    local ret = destructure1(to, nil, ast, true)
    applyManglings(scope, newManglings, ast)
    return ret
  end
  local function requireInclude(ast, scope, parent, opts)
    opts.fallback = function(e)
      return utils.expr(("require(%s)"):format(tostring(e)), "statement")
    end
    return scopes.global.specials.include(ast, scope, parent, opts)
  end
  local function compileStream(strm, options)
    local opts = utils.copy(options)
    local oldGlobals = allowedGlobals
    local scope = (opts.scope or makeScope(scopes.global))
    local vals = {}
    local chunk = {}
    do end (utils.root):setReset()
    allowedGlobals = opts.allowedGlobals
    if (opts.indent == nil) then
      opts.indent = "  "
    end
    if opts.requireAsInclude then
      scope.specials.require = requireInclude
    end
    utils.root.chunk, utils.root.scope, utils.root.options = chunk, scope, opts
    for ok, val in parser.parser(strm, opts.filename, opts) do
      vals[(#vals + 1)] = val
    end
    for i = 1, #vals, 1 do
      local exprs = compile1(vals[i], scope, chunk, {nval = (((i < #vals) and 0) or nil), tail = (i == #vals)})
      keepSideEffects(exprs, chunk, nil, vals[i])
    end
    allowedGlobals = oldGlobals
    utils.root.reset()
    return flatten(chunk, opts)
  end
  local function compileString(str, opts)
    return compileStream(parser.stringStream(str), (opts or {}))
  end
  local function compile(ast, opts)
    local opts0 = utils.copy(opts)
    local oldGlobals = allowedGlobals
    local chunk = {}
    local scope = (opts0.scope or makeScope(scopes.global))
    do end (utils.root):setReset()
    allowedGlobals = opts0.allowedGlobals
    if (opts0.indent == nil) then
      opts0.indent = "  "
    end
    if opts0.requireAsInclude then
      scope.specials.require = requireInclude
    end
    utils.root.chunk, utils.root.scope, utils.root.options = chunk, scope, opts0
    local exprs = compile1(ast, scope, chunk, {tail = true})
    keepSideEffects(exprs, chunk, nil, ast)
    allowedGlobals = oldGlobals
    utils.root.reset()
    return flatten(chunk, opts0)
  end
  local function traceback_frame(info)
    if ((info.what == "C") and info.name) then
      return ("  [C]: in function '%s'"):format(info.name)
    elseif (info.what == "C") then
      return "  [C]: in ?"
    else
      local remap = fennelSourcemap[info.source]
      if (remap and remap[info.currentline]) then
        info.short_src = remap.short_src
        info.currentline = remap[info.currentline]
      end
      if (info.what == "Lua") then
        local function _1_()
          if info.name then
            return ("'" .. info.name .. "'")
          else
            return "?"
          end
        end
        return ("  %s:%d: in function %s"):format(info.short_src, info.currentline, _1_())
      elseif (info.short_src == "(tail call)") then
        return "  (tail call)"
      else
        return ("  %s:%d: in main chunk"):format(info.short_src, info.currentline)
      end
    end
  end
  local function traceback(msg, start)
    local msg0 = (msg or "")
    if ((msg0:find("^Compile error") or msg0:find("^Parse error")) and not utils.debugOn("trace")) then
      return msg0
    else
      local lines = {}
      if (msg0:find("^Compile error") or msg0:find("^Parse error")) then
        table.insert(lines, msg0)
      else
        local newmsg = msg0:gsub("^[^:]*:%d+:%s+", "runtime error: ")
        table.insert(lines, newmsg)
      end
      table.insert(lines, "stack traceback:")
      local done_3f, level = false, (start or 2)
      while not done_3f do
        do
          local _1_0 = debug.getinfo(level, "Sln")
          if (_1_0 == nil) then
            done_3f = true
          elseif (nil ~= _1_0) then
            local info = _1_0
            table.insert(lines, traceback_frame(info))
          end
        end
        level = (level + 1)
      end
      return table.concat(lines, "\n")
    end
  end
  local function entryTransform(fk, fv)
    local function _0_(k, v)
      if (type(k) == "number") then
        return k, fv(v)
      else
        return fk(k), fv(v)
      end
    end
    return _0_
  end
  local function no()
    return nil
  end
  local function mixedConcat(t, joiner)
    local seen = {}
    local ret, s = "", ""
    for k, v in ipairs(t) do
      table.insert(seen, k)
      ret = (ret .. s .. v)
      s = joiner
    end
    for k, v in utils.stablepairs(t) do
      if not seen[k] then
        ret = (ret .. s .. "[" .. k .. "]" .. "=" .. v)
        s = joiner
      end
    end
    return ret
  end
  local function doQuote(form, scope, parent, runtime)
    local function q(x)
      return doQuote(x, scope, parent, runtime)
    end
    if utils.isVarg(form) then
      assertCompile(not runtime, "quoted ... may only be used at compile time", form)
      return "_VARARG"
    elseif utils.isSym(form) then
      local filename = nil
      if form.filename then
        filename = ("%q"):format(form.filename)
      else
        filename = "nil"
      end
      local symstr = utils.deref(form)
      assertCompile(not runtime, "symbols may only be used at compile time", form)
      if (symstr:find("#$") or symstr:find("#[:.]")) then
        return ("sym('%s', nil, {filename=%s, line=%s})"):format(autogensym(symstr, scope), filename, (form.line or "nil"))
      else
        return ("sym('%s', nil, {quoted=true, filename=%s, line=%s})"):format(symstr, filename, (form.line or "nil"))
      end
    elseif (utils.isList(form) and utils.isSym(form[1]) and (utils.deref(form[1]) == "unquote")) then
      local payload = form[2]
      local res = unpack(compile1(payload, scope, parent))
      return res[1]
    elseif utils.isList(form) then
      local mapped = utils.kvmap(form, entryTransform(no, q))
      local filename = nil
      if form.filename then
        filename = ("%q"):format(form.filename)
      else
        filename = "nil"
      end
      assertCompile(not runtime, "lists may only be used at compile time", form)
      return (("setmetatable({filename=%s, line=%s, bytestart=%s, %s}" .. ", getmetatable(list()))")):format(filename, (form.line or "nil"), (form.bytestart or "nil"), mixedConcat(mapped, ", "))
    elseif (type(form) == "table") then
      local mapped = utils.kvmap(form, entryTransform(q, q))
      local source = getmetatable(form)
      local filename = nil
      if source.filename then
        filename = ("%q"):format(source.filename)
      else
        filename = "nil"
      end
      local function _1_()
        if source then
          return source.line
        else
          return "nil"
        end
      end
      return ("setmetatable({%s}, {filename=%s, line=%s})"):format(mixedConcat(mapped, ", "), filename, _1_())
    elseif (type(form) == "string") then
      return serializeString(form)
    else
      return tostring(form)
    end
  end
  return {applyManglings = applyManglings, assert = assertCompile, autogensym = autogensym, compile = compile, compile1 = compile1, compileStream = compileStream, compileString = compileString, declareLocal = declareLocal, destructure = destructure, doQuote = doQuote, emit = emit, gensym = gensym, globalMangling = globalMangling, globalUnmangling = globalUnmangling, keepSideEffects = keepSideEffects, macroexpand = macroexpand_2a, makeScope = makeScope, metadata = makeMetadata(), requireInclude = requireInclude, scopes = scopes, symbolToExpression = symbolToExpression, traceback = traceback}
end
package.preload["fennel.friend"] = package.preload["fennel.friend"] or function(...)
  local function ast_source(ast)
    local m = getmetatable(ast)
    if (m and m.line and m) then
      return m
    else
      return ast
    end
  end
  local suggestions = {["$ and $... in hashfn are mutually exclusive"] = {"modifying the hashfn so it only contains $... or $, $1, $2, $3, etc"}, ["can't start multisym segment with a digit"] = {"removing the digit", "adding a non-digit before the digit"}, ["cannot call literal value"] = {"checking for typos", "checking for a missing function name"}, ["could not compile value of type "] = {"debugging the macro you're calling not to return a coroutine or userdata"}, ["could not read number (.*)"] = {"removing the non-digit character", "beginning the identifier with a non-digit if it is not meant to be a number"}, ["expected a function.* to call"] = {"removing the empty parentheses", "using square brackets if you want an empty table"}, ["expected binding table"] = {"placing a table here in square brackets containing identifiers to bind"}, ["expected body expression"] = {"putting some code in the body of this form after the bindings"}, ["expected each macro to be function"] = {"ensuring that the value for each key in your macros table contains a function", "avoid defining nested macro tables"}, ["expected even number of name/value bindings"] = {"finding where the identifier or value is missing"}, ["expected even number of values in table literal"] = {"removing a key", "adding a value"}, ["expected local"] = {"looking for a typo", "looking for a local which is used out of its scope"}, ["expected macros to be table"] = {"ensuring your macro definitions return a table"}, ["expected parameters"] = {"adding function parameters as a list of identifiers in brackets"}, ["expected rest argument before last parameter"] = {"moving & to right before the final identifier when destructuring"}, ["expected symbol for function parameter: (.*)"] = {"changing %s to an identifier instead of a literal value"}, ["expected var (.*)"] = {"declaring %s using var instead of let/local", "introducing a new local instead of changing the value of %s"}, ["expected vararg as last parameter"] = {"moving the \"...\" to the end of the parameter list"}, ["expected whitespace before opening delimiter"] = {"adding whitespace"}, ["global (.*) conflicts with local"] = {"renaming local %s"}, ["illegal character: (.)"] = {"deleting or replacing %s", "avoiding reserved characters like \", \\, ', ~, ;, @, `, and comma"}, ["local (.*) was overshadowed by a special form or macro"] = {"renaming local %s"}, ["macro not found in macro module"] = {"checking the keys of the imported macro module's returned table"}, ["macro tried to bind (.*) without gensym"] = {"changing to %s# when introducing identifiers inside macros"}, ["malformed multisym"] = {"ensuring each period or colon is not followed by another period or colon"}, ["may only be used at compile time"] = {"moving this to inside a macro if you need to manipulate symbols/lists", "using square brackets instead of parens to construct a table"}, ["method must be last component"] = {"using a period instead of a colon for field access", "removing segments after the colon", "making the method call, then looking up the field on the result"}, ["mismatched closing delimiter (.), expected (.)"] = {"replacing %s with %s", "deleting %s", "adding matching opening delimiter earlier"}, ["multisym method calls may only be in call position"] = {"using a period instead of a colon to reference a table's fields", "putting parens around this"}, ["unable to bind (.*)"] = {"replacing the %s with an identifier"}, ["unexpected closing delimiter (.)"] = {"deleting %s", "adding matching opening delimiter earlier"}, ["unexpected multi symbol (.*)"] = {"removing periods or colons from %s"}, ["unexpected vararg"] = {"putting \"...\" at the end of the fn parameters if the vararg was intended"}, ["unknown global in strict mode: (.*)"] = {"looking to see if there's a typo", "using the _G table instead, eg. _G.%s if you really want a global", "moving this code to somewhere that %s is in scope", "binding %s as a local in the scope of this code"}, ["unused local (.*)"] = {"fixing a typo so %s is used", "renaming the local to _%s"}, ["use of global (.*) is aliased by a local"] = {"renaming local %s", "refer to the global using _G.%s instead of directly"}}
  local unpack = (_G.unpack or table.unpack)
  local function suggest(msg)
    local suggestion = nil
    for pat, sug in pairs(suggestions) do
      local matches = {msg:match(pat)}
      if (0 < #matches) then
        if ("table" == type(sug)) then
          local out = {}
          for _, s in ipairs(sug) do
            table.insert(out, s:format(unpack(matches)))
          end
          suggestion = out
        else
          suggestion = sug(matches)
        end
      end
    end
    return suggestion
  end
  local function read_line_from_file(filename, line)
    local bytes = 0
    local f = assert(io.open(filename))
    local _ = nil
    for _0 = 1, (line - 1) do
      bytes = (bytes + 1 + #f:read())
    end
    _ = nil
    local codeline = f:read()
    f:close()
    return codeline, bytes
  end
  local function read_line_from_source(source, line)
    local lines, bytes, codeline = 0, 0
    for this_line, newline in string.gmatch((source .. "\n"), "(.-)(\13?\n)") do
      lines = (lines + 1)
      if (lines == line) then
        codeline = this_line
        break
      end
      bytes = (bytes + #newline + #this_line)
    end
    return codeline, bytes
  end
  local function read_line(filename, line, source)
    if source then
      return read_line_from_source(source, line)
    else
      return read_line_from_file(filename, line)
    end
  end
  local function friendly_msg(msg, _0_0, source)
    local _1_ = _0_0
    local byteend = _1_["byteend"]
    local bytestart = _1_["bytestart"]
    local filename = _1_["filename"]
    local line = _1_["line"]
    local ok, codeline, bol, eol = pcall(read_line, filename, line, source)
    local suggestions0 = suggest(msg)
    local out = {msg, ""}
    if (ok and codeline) then
      table.insert(out, codeline)
    end
    if (ok and codeline and bytestart and byteend) then
      table.insert(out, (string.rep(" ", (bytestart - bol - 1)) .. "^" .. string.rep("^", math.min((byteend - bytestart), ((bol + #codeline) - bytestart)))))
    end
    if (ok and codeline and bytestart and not byteend) then
      table.insert(out, (string.rep("-", (bytestart - bol - 1)) .. "^"))
      table.insert(out, "")
    end
    if suggestions0 then
      for _, suggestion in ipairs(suggestions0) do
        table.insert(out, ("* Try %s."):format(suggestion))
      end
    end
    return table.concat(out, "\n")
  end
  local function assert_compile(condition, msg, ast, source)
    if not condition then
      local _1_ = ast_source(ast)
      local filename = _1_["filename"]
      local line = _1_["line"]
      error(friendly_msg(("Compile error in %s:%s\n  %s"):format((filename or "unknown"), (line or "?"), msg), ast_source(ast), source), 0)
    end
    return condition
  end
  local function parse_error(msg, filename, line, bytestart, source)
    return error(friendly_msg(("Parse error in %s:%s\n  %s"):format(filename, line, msg), {bytestart = bytestart, filename = filename, line = line}, source), 0)
  end
  return {["assert-compile"] = assert_compile, ["parse-error"] = parse_error}
end
package.preload["fennel.parser"] = package.preload["fennel.parser"] or function(...)
  local utils = require("fennel.utils")
  local friend = require("fennel.friend")
  local unpack = (_G.unpack or table.unpack)
  local function granulate(getchunk)
    local c = ""
    local index = 1
    local done = false
    local function _0_(parserState)
      if not done then
        if (index <= #c) then
          local b = c:byte(index)
          index = (index + 1)
          return b
        else
          c = getchunk(parserState)
          if (not c or (c == "")) then
            done = true
            return nil
          end
          index = 2
          return c:byte(1)
        end
      end
    end
    local function _1_()
      c = ""
      return nil
    end
    return _0_, _1_
  end
  local function stringStream(str)
    local str0 = str:gsub("^#![^\n]*\n", "")
    local index = 1
    local function _0_()
      local r = str0:byte(index)
      index = (index + 1)
      return r
    end
    return _0_
  end
  local delims = {[123] = 125, [125] = true, [40] = 41, [41] = true, [91] = 93, [93] = true}
  local function iswhitespace(b)
    return ((b == 32) or ((b >= 9) and (b <= 13)))
  end
  local function issymbolchar(b)
    return ((b > 32) and not delims[b] and (b ~= 127) and (b ~= 34) and (b ~= 39) and (b ~= 126) and (b ~= 59) and (b ~= 44) and (b ~= 64) and (b ~= 96))
  end
  local prefixes = {[35] = "hashfn", [39] = "quote", [44] = "unquote", [96] = "quote"}
  local function parser(getbyte, filename, options)
    local stack = {}
    local line = 1
    local byteindex = 0
    local lastb = nil
    local function ungetb(ub)
      if (ub == 10) then
        line = (line - 1)
      end
      byteindex = (byteindex - 1)
      lastb = ub
      return nil
    end
    local function getb()
      local r = nil
      if lastb then
        r, lastb = lastb, nil
      else
        r = getbyte({stackSize = #stack})
      end
      byteindex = (byteindex + 1)
      if (r == 10) then
        line = (line + 1)
      end
      return r
    end
    local function parseError(msg)
      local _0_ = (utils.root.options or {})
      local source = _0_["source"]
      local unfriendly = _0_["unfriendly"]
      utils.root.reset()
      if unfriendly then
        return error(("Parse error in %s:%s: %s"):format((filename or "unknown"), (line or "?"), msg), 0)
      else
        return friend["parse-error"](msg, (filename or "unknown"), (line or "?"), byteindex, source)
      end
    end
    local function parseStream()
      local whitespaceSinceDispatch, done, retval = true
      local function dispatch(v)
        if (#stack == 0) then
          retval = v
          done = true
          whitespaceSinceDispatch = false
          return nil
        elseif stack[#stack].prefix then
          local stacktop = stack[#stack]
          stack[#stack] = nil
          return dispatch(utils.list(utils.sym(stacktop.prefix), v))
        else
          whitespaceSinceDispatch = false
          return table.insert(stack[#stack], v)
        end
      end
      local function badend()
        local accum = utils.map(stack, "closer")
        return parseError(("expected closing delimiter%s %s"):format((((#stack == 1) and "") or "s"), string.char(unpack(accum))))
      end
      while true do
        local b = nil
        while true do
          b = getb()
          if (b and iswhitespace(b)) then
            whitespaceSinceDispatch = true
          end
          if (not b or not iswhitespace(b)) then
            break
          end
        end
        if not b then
          if (#stack > 0) then
            badend()
          end
          return nil
        end
        if (b == 59) then
          while true do
            b = getb()
            if (not b or (b == 10)) then
              break
            end
          end
        elseif (type(delims[b]) == "number") then
          if not whitespaceSinceDispatch then
            parseError(("expected whitespace before opening delimiter " .. string.char(b)))
          end
          table.insert(stack, setmetatable({bytestart = byteindex, closer = delims[b], filename = filename, line = line}, getmetatable(utils.list())))
        elseif delims[b] then
          if (#stack == 0) then
            parseError(("unexpected closing delimiter " .. string.char(b)))
          end
          local last = stack[#stack]
          local val = nil
          if (last.closer ~= b) then
            parseError(("mismatched closing delimiter " .. string.char(b) .. ", expected " .. string.char(last.closer)))
          end
          last.byteend = byteindex
          if (b == 41) then
            val = last
          elseif (b == 93) then
            val = utils.sequence(unpack(last))
            for k, v in pairs(last) do
              getmetatable(val)[k] = v
            end
          else
            if ((#last % 2) ~= 0) then
              byteindex = (byteindex - 1)
              parseError("expected even number of values in table literal")
            end
            val = {}
            setmetatable(val, last)
            for i = 1, #last, 2 do
              if ((tostring(last[i]) == ":") and utils.isSym(last[(i + 1)]) and utils.isSym(last[i])) then
                last[i] = tostring(last[(i + 1)])
              end
              val[last[i]] = last[(i + 1)]
            end
          end
          stack[#stack] = nil
          dispatch(val)
        elseif (b == 34) then
          local state = "base"
          local chars = {34}
          stack[(#stack + 1)] = {closer = 34}
          while true do
            b = getb()
            chars[(#chars + 1)] = b
            if (state == "base") then
              if (b == 92) then
                state = "backslash"
              elseif (b == 34) then
                state = "done"
              end
            else
              state = "base"
            end
            if (not b or (state == "done")) then
              break
            end
          end
          if not b then
            badend()
          end
          stack[#stack] = nil
          local raw = string.char(unpack(chars))
          local formatted = nil
          local function _2_(c)
            return ("\\" .. c:byte())
          end
          formatted = raw:gsub("[\1-\31]", _2_)
          local loadFn = (_G.loadstring or load)(("return %s"):format(formatted))
          dispatch(loadFn())
        elseif prefixes[b] then
          table.insert(stack, {prefix = prefixes[b]})
          local nextb = getb()
          if iswhitespace(nextb) then
            if (b ~= 35) then
              parseError("invalid whitespace after quoting prefix")
            end
            stack[#stack] = nil
            dispatch(utils.sym("#"))
          end
          ungetb(nextb)
        elseif (issymbolchar(b) or (b == string.byte("~"))) then
          local chars = {}
          local bytestart = byteindex
          while true do
            chars[(#chars + 1)] = b
            b = getb()
            if (not b or not issymbolchar(b)) then
              break
            end
          end
          if b then
            ungetb(b)
          end
          local rawstr = string.char(unpack(chars))
          if (rawstr == "true") then
            dispatch(true)
          elseif (rawstr == "false") then
            dispatch(false)
          elseif (rawstr == "...") then
            dispatch(utils.varg())
          elseif rawstr:match("^:.+$") then
            dispatch(rawstr:sub(2))
          elseif (rawstr:match("^~") and (rawstr ~= "~=")) then
            parseError("illegal character: ~")
          else
            local forceNumber = rawstr:match("^%d")
            local numberWithStrippedUnderscores = rawstr:gsub("_", "")
            local x = nil
            if forceNumber then
              x = (tonumber(numberWithStrippedUnderscores) or parseError(("could not read number \"" .. rawstr .. "\"")))
            else
              x = tonumber(numberWithStrippedUnderscores)
              if not x then
                if rawstr:match("%.[0-9]") then
                  byteindex = (((byteindex - #rawstr) + rawstr:find("%.[0-9]")) + 1)
                  parseError(("can't start multisym segment " .. "with a digit: " .. rawstr))
                elseif (rawstr:match("[%.:][%.:]") and (rawstr ~= "..") and (rawstr ~= "$...")) then
                  byteindex = ((byteindex - #rawstr) + 1 + rawstr:find("[%.:][%.:]"))
                  parseError(("malformed multisym: " .. rawstr))
                elseif rawstr:match(":.+[%.:]") then
                  byteindex = ((byteindex - #rawstr) + rawstr:find(":.+[%.:]"))
                  parseError(("method must be last component " .. "of multisym: " .. rawstr))
                else
                  x = utils.sym(rawstr, nil, {byteend = byteindex, bytestart = bytestart, filename = filename, line = line})
                end
              end
            end
            dispatch(x)
          end
        else
          parseError(("illegal character: " .. string.char(b)))
        end
        if done then
          break
        end
      end
      return true, retval
    end
    local function _0_()
      stack = {}
      return nil
    end
    return parseStream, _0_
  end
  return {granulate = granulate, parser = parser, stringStream = stringStream}
end
local utils = nil
package.preload["fennel.utils"] = package.preload["fennel.utils"] or function(...)
  local function stablepairs(t)
    local keys = {}
    local succ = {}
    for k in pairs(t) do
      table.insert(keys, k)
    end
    local function _0_(a, b)
      return (tostring(a) < tostring(b))
    end
    table.sort(keys, _0_)
    for i, k in ipairs(keys) do
      succ[k] = keys[(i + 1)]
    end
    local function stablenext(tbl, idx)
      if (idx == nil) then
        return keys[1], tbl[keys[1]]
      else
        return succ[idx], tbl[succ[idx]]
      end
    end
    return stablenext, t, nil
  end
  local function map(t, f, out)
    local out0 = (out or {})
    local f0 = nil
    if (type(f) == "function") then
      f0 = f
    else
      local s = f
      local function _0_(x)
        return x[s]
      end
      f0 = _0_
    end
    for _, x in ipairs(t) do
      local _1_0 = f0(x)
      if (nil ~= _1_0) then
        local v = _1_0
        table.insert(out0, v)
      end
    end
    return out0
  end
  local function kvmap(t, f, out)
    local out0 = (out or {})
    local f0 = nil
    if (type(f) == "function") then
      f0 = f
    else
      local s = f
      local function _0_(x)
        return x[s]
      end
      f0 = _0_
    end
    for k, x in stablepairs(t) do
      local korv, v = f0(k, x)
      if (korv and not v) then
        table.insert(out0, korv)
      end
      if (korv and v) then
        out0[korv] = v
      end
    end
    return out0
  end
  local function copy(from)
    local to = {}
    for k, v in pairs((from or {})) do
      to[k] = v
    end
    return to
  end
  local function allpairs(tbl)
    assert((type(tbl) == "table"), "allpairs expects a table")
    local t = tbl
    local seen = {}
    local function allpairsNext(_, state)
      local nextState, value = next(t, state)
      if seen[nextState] then
        return allpairsNext(nil, nextState)
      elseif nextState then
        seen[nextState] = true
        return nextState, value
      else
        local meta = getmetatable(t)
        if (meta and meta.__index) then
          t = meta.__index
          return allpairsNext(t)
        end
      end
    end
    return allpairsNext
  end
  local function deref(self)
    return self[1]
  end
  local nilSym = nil
  local function listToString(self, tostring2)
    local safe, max = {}, 0
    for k in pairs(self) do
      if ((type(k) == "number") and (k > max)) then
        max = k
      end
    end
    for i = 1, max, 1 do
      safe[i] = (((self[i] == nil) and nilSym) or self[i])
    end
    return ("(" .. table.concat(map(safe, (tostring2 or tostring)), " ", 1, max) .. ")")
  end
  local SYMBOL_MT = {"SYMBOL", __fennelview = deref, __tostring = deref}
  local EXPR_MT = {"EXPR", __tostring = deref}
  local LIST_MT = {"LIST", __fennelview = listToString, __tostring = listToString}
  local SEQUENCE_MARKER = {"SEQUENCE"}
  local VARARG = setmetatable({"..."}, {"VARARG", __fennelview = deref, __tostring = deref})
  local getenv = nil
  local function _0_()
    return nil
  end
  getenv = ((os and os.getenv) or _0_)
  local function debugOn(flag)
    local level = (getenv("FENNEL_DEBUG") or "")
    return ((level == "all") or level:find(flag))
  end
  local function list(...)
    return setmetatable({...}, LIST_MT)
  end
  local function sym(str, scope, source)
    local s = {str, scope = scope}
    for k, v in pairs((source or {})) do
      if (type(k) == "string") then
        s[k] = v
      end
    end
    return setmetatable(s, SYMBOL_MT)
  end
  nilSym = sym("nil")
  local function sequence(...)
    return setmetatable({...}, {sequence = SEQUENCE_MARKER})
  end
  local function expr(strcode, etype)
    return setmetatable({strcode, type = etype}, EXPR_MT)
  end
  local function varg()
    return VARARG
  end
  local function isExpr(x)
    return ((type(x) == "table") and (getmetatable(x) == EXPR_MT) and x)
  end
  local function isVarg(x)
    return ((x == VARARG) and x)
  end
  local function isList(x)
    return ((type(x) == "table") and (getmetatable(x) == LIST_MT) and x)
  end
  local function isSym(x)
    return ((type(x) == "table") and (getmetatable(x) == SYMBOL_MT) and x)
  end
  local function isTable(x)
    return ((type(x) == "table") and (x ~= VARARG) and (getmetatable(x) ~= LIST_MT) and (getmetatable(x) ~= SYMBOL_MT) and x)
  end
  local function isSequence(x)
    local mt = ((type(x) == "table") and getmetatable(x))
    return (mt and (mt.sequence == SEQUENCE_MARKER) and x)
  end
  local function isMultiSym(str)
    if isSym(str) then
      return isMultiSym(tostring(str))
    elseif (type(str) ~= "string") then
      return false
    else
      local parts = {}
      for part in str:gmatch("[^%.%:]+[%.%:]?") do
        local lastChar = part:sub(( - 1))
        if (lastChar == ":") then
          parts.multiSymMethodCall = true
        end
        if ((lastChar == ":") or (lastChar == ".")) then
          parts[(#parts + 1)] = part:sub(1, ( - 2))
        else
          parts[(#parts + 1)] = part
        end
      end
      return ((#parts > 0) and (str:match("%.") or str:match(":")) and not str:match("%.%.") and (str:byte() ~= string.byte(".")) and (str:byte(( - 1)) ~= string.byte(".")) and parts)
    end
  end
  local function isQuoted(symbol)
    return symbol.quoted
  end
  local function walkTree(root, f, customIterator)
    local function walk(iterfn, parent, idx, node)
      if f(idx, node, parent) then
        for k, v in iterfn(node) do
          walk(iterfn, node, k, v)
        end
        return nil
      end
    end
    walk((customIterator or pairs), nil, nil, root)
    return root
  end
  local luaKeywords = {"and", "break", "do", "else", "elseif", "end", "false", "for", "function", "if", "in", "local", "nil", "not", "or", "repeat", "return", "then", "true", "until", "while"}
  for i, v in ipairs(luaKeywords) do
    luaKeywords[v] = i
  end
  local function isValidLuaIdentifier(str)
    return (str:match("^[%a_][%w_]*$") and not luaKeywords[str])
  end
  local propagatedOptions = {"allowedGlobals", "indent", "correlate", "useMetadata", "env"}
  local function propagateOptions(options, subopts)
    for _, name in ipairs(propagatedOptions) do
      subopts[name] = options[name]
    end
    return subopts
  end
  local root = nil
  local function _1_()
  end
  root = {chunk = nil, options = nil, reset = _1_, scope = nil}
  root.setReset = function(root0)
    local _2_ = root0
    local chunk = _2_["chunk"]
    local options = _2_["options"]
    local reset = _2_["reset"]
    local scope = _2_["scope"]
    root0.reset = function()
      local function _3_()
        root0.chunk, root0.scope, root0.options, root0.reset = chunk, scope, options, reset
        return nil
      end
      return _3_
    end
    return root0.reset
  end
  local _3_
  do
    local _2_0 = {"./?.fnl", "./?/init.fnl"}
    table.insert(_2_0, getenv("FENNEL_PATH"))
    _3_ = _2_0
  end
  return {allpairs = allpairs, copy = copy, debugOn = debugOn, deref = deref, expr = expr, isExpr = isExpr, isList = isList, isMultiSym = isMultiSym, isQuoted = isQuoted, isSequence = isSequence, isSym = isSym, isTable = isTable, isValidLuaIdentifier = isValidLuaIdentifier, isVarg = isVarg, kvmap = kvmap, list = list, luaKeywords = luaKeywords, map = map, path = table.concat(_3_, ";"), propagateOptions = propagateOptions, root = root, sequence = sequence, stablepairs = stablepairs, sym = sym, varg = varg, walkTree = walkTree}
end
utils = require("fennel.utils")
local parser = require("fennel.parser")
local compiler = require("fennel.compiler")
local specials = require("fennel.specials")
local repl = require("fennel.repl")
local function eval(str, options, ...)
  local opts = utils.copy(options)
  local _ = nil
  if ((opts.allowedGlobals == nil) and not getmetatable(opts.env)) then
    opts.allowedGlobals = specials.currentGlobalNames(opts.env)
    _ = nil
  else
  _ = nil
  end
  local env = (opts.env and specials.wrapEnv(opts.env))
  local lua_source = compiler.compileString(str, opts)
  local loader = nil
  local function _1_(...)
    if opts.filename then
      return ("@" .. opts.filename)
    else
      return str
    end
  end
  loader = specials.loadCode(lua_source, env, _1_(...))
  opts.filename = nil
  return loader(...)
end
local function dofile_2a(filename, options, ...)
  local opts = utils.copy(options)
  local f = assert(io.open(filename, "rb"))
  local source = f:read("*all")
  f:close()
  opts.filename = filename
  return eval(source, opts, ...)
end
local mod = {compile = compiler.compile, compile1 = compiler.compile1, compileStream = compiler.compileStream, compileString = compiler.compileString, doc = specials.doc, dofile = dofile_2a, eval = eval, gensym = compiler.gensym, granulate = parser.grandulate, list = utils.list, loadCode = specials.loadCode, macroLoaded = specials.macroLoaded, makeSearcher = specials.makeSearcher, make_searcher = specials.makeSearcher, mangle = compiler.globalMangling, metadata = compiler.metadata, parser = parser.parser, path = utils.path, repl = repl, scope = compiler.makeScope, searchModule = specials.searchModule, searcher = specials.makeSearcher(), stringStream = parser.stringStream, sym = utils.sym, traceback = compiler.traceback, unmangle = compiler.globalUnmangling, varg = utils.varg, version = "0.5.0-dev"}
utils.fennelModule = mod
do
  local builtin_macros = [===[;; The code for these macros is somewhat idiosyncratic because it cannot use any
  ;; macros which have not yet been defined.
  
  (fn -> [val ...]
    "Thread-first macro.
  Take the first value and splice it into the second form as its first argument.
  The value of the second form is spliced into the first arg of the third, etc."
    (var x val)
    (each [_ e (ipairs [...])]
      (let [elt (if (list? e) e (list e))]
        (table.insert elt 2 x)
        (set x elt)))
    x)
  
  (fn ->> [val ...]
    "Thread-last macro.
  Same as ->, except splices the value into the last position of each form
  rather than the first."
    (var x val)
    (each [_ e (pairs [...])]
      (let [elt (if (list? e) e (list e))]
        (table.insert elt x)
        (set x elt)))
    x)
  
  (fn -?> [val ...]
    "Nil-safe thread-first macro.
  Same as -> except will short-circuit with nil when it encounters a nil value."
    (if (= 0 (select "#" ...))
        val
        (let [els [...]
              e (table.remove els 1)
              el (if (list? e) e (list e))
              tmp (gensym)]
          (table.insert el 2 tmp)
          `(let [,tmp ,val]
             (if ,tmp
                 (-?> ,el ,(unpack els))
                 ,tmp)))))
  
  (fn -?>> [val ...]
    "Nil-safe thread-last macro.
  Same as ->> except will short-circuit with nil when it encounters a nil value."
    (if (= 0 (select "#" ...))
        val
        (let [els [...]
              e (table.remove els 1)
              el (if (list? e) e (list e))
              tmp (gensym)]
          (table.insert el tmp)
          `(let [,tmp ,val]
             (if ,tmp
                 (-?>> ,el ,(unpack els))
                 ,tmp)))))
  
  (fn doto [val ...]
    "Evaluates val and splices it into the first argument of subsequent forms."
    (let [name (gensym)
          form `(let [,name ,val])]
      (each [_ elt (pairs [...])]
        (table.insert elt 2 name)
        (table.insert form elt))
      (table.insert form name)
      form))
  
  (fn when [condition body1 ...]
    "Evaluate body for side-effects only when condition is truthy."
    (assert body1 "expected body")
    `(if ,condition
         (do ,body1 ,...)))
  
  (fn with-open [closable-bindings ...]
    "Like `let`, but invokes (v:close) on each binding after evaluating the body.
  The body is evaluated inside `xpcall` so that bound values will be closed upon
  encountering an error before propagating it."
    (let [bodyfn    `(fn [] ,...)
          closer `(fn close-handlers# [ok# ...] (if ok# ...
                                                    (error ... 0)))
          traceback `(. (or package.loaded.fennel debug) :traceback)]
      (for [i 1 (# closable-bindings) 2]
        (assert (sym? (. closable-bindings i))
                "with-open only allows symbols in bindings")
        (table.insert closer 4 `(: ,(. closable-bindings i) :close)))
      `(let ,closable-bindings ,closer
            (close-handlers# (xpcall ,bodyfn ,traceback)))))
  
  (fn partial [f ...]
    "Returns a function with all arguments partially applied to f."
    (let [body (list f ...)]
      (table.insert body _VARARG)
      `(fn [,_VARARG] ,body)))
  
  (fn pick-args [n f]
    "Creates a function of arity n that applies its arguments to f.
  
  For example,
    (pick-args 2 func)
  expands to
    (fn [_0_ _1_] (func _0_ _1_))"
    (assert (and (= (type n) :number) (= n (math.floor n)) (>= n 0))
            "Expected n to be an integer literal >= 0.")
    (let [bindings []]
      (for [i 1 n] (tset bindings i (gensym)))
      `(fn ,bindings (,f ,(unpack bindings)))))
  
  (fn pick-values [n ...]
    "Like the `values` special, but emits exactly n values.
  
  For example,
    (pick-values 2 ...)
  expands to
    (let [(_0_ _1_) ...]
      (values _0_ _1_))"
    (assert (and (= :number (type n)) (>= n 0) (= n (math.floor n)))
            "Expected n to be an integer >= 0")
    (let [let-syms   (list)
          let-values (if (= 1 (select :# ...)) ... `(values ,...))]
      (for [i 1 n] (table.insert let-syms (gensym)))
      (if (= n 0) `(values)
          `(let [,let-syms ,let-values] (values ,(unpack let-syms))))))
  
  (fn lambda [...]
    "Function literal with arity checking.
  Will throw an exception if a declared argument is passed in as nil, unless
  that argument name begins with ?."
    (let [args [...]
          has-internal-name? (sym? (. args 1))
          arglist (if has-internal-name? (. args 2) (. args 1))
          docstring-position (if has-internal-name? 3 2)
          has-docstring? (and (> (# args) docstring-position)
                              (= :string (type (. args docstring-position))))
          arity-check-position (- 4 (if has-internal-name? 0 1)
                                  (if has-docstring? 0 1))]
      (fn check! [a]
        (if (table? a)
            (each [_ a (pairs a)]
              (check! a))
            (and (not (: (tostring a) :match "^?"))
                 (not= (tostring a) "&")
                 (not= (tostring a) "..."))
            (table.insert args arity-check-position
                          `(assert (not= nil ,a)
                                   (: "Missing argument %s on %s:%s"
                                      :format ,(tostring a)
                                      ,(or a.filename "unknown")
                                      ,(or a.line "?"))))))
      (assert (> (length args) 1) "expected body expression")
      (each [_ a (ipairs arglist)]
        (check! a))
      `(fn ,(unpack args))))
  
  (fn macro [name ...]
    "Define a single macro."
    (assert (sym? name) "expected symbol for macro name")
    (local args [...])
    `(macros { ,(tostring name) (fn ,name ,(unpack args))}))
  
  (fn macrodebug [form return?]
    "Print the resulting form after performing macroexpansion.
  With a second argument, returns expanded form as a string instead of printing."
    (let [(ok view) (pcall require :fennelview)
          handle (if return? `do `print)]
      `(,handle ,((if ok view tostring) (macroexpand form _SCOPE)))))
  
  (fn import-macros [binding1 module-name1 ...]
    "Binds a table of macros from each macro module according to a binding form.
  Each binding form can be either a symbol or a k/v destructuring table.
  Example:
    (import-macros mymacros                 :my-macros    ; bind to symbol
                   {:macro1 alias : macro2} :proj.macros) ; import by name"
    (assert (and binding1 module-name1 (= 0 (% (select :# ...) 2)))
            "expected even number of binding/modulename pairs")
    (for [i 1 (select :# binding1 module-name1 ...) 2]
      (local (binding modname) (select i binding1 module-name1 ...))
      ;; generate a subscope of current scope, use require-macros
      ;; to bring in macro module. after that, we just copy the
      ;; macros from subscope to scope.
      (local scope (get-scope))
      (local subscope (fennel.scope scope))
      (fennel.compileString (string.format "(require-macros %q)"
                                           modname)
                            {:scope subscope})
      (if (sym? binding)
          ;; bind whole table of macros to table bound to symbol
          (do (tset scope.macros (. binding 1) {})
              (each [k v (pairs subscope.macros)]
                (tset (. scope.macros (. binding 1)) k v)))
  
          ;; 1-level table destructuring for importing individual macros
          (table? binding)
          (each [macro-name [import-key] (pairs binding)]
            (assert (= :function (type (. subscope.macros macro-name)))
                    (.. "macro " macro-name " not found in module " modname))
            (tset scope.macros import-key (. subscope.macros macro-name)))))
    nil)
  
  ;;; Pattern matching
  
  (fn match-pattern [vals pattern unifications]
    "Takes the AST of values and a single pattern and returns a condition
  to determine if it matches as well as a list of bindings to
  introduce for the duration of the body if it does match."
    ;; we have to assume we're matching against multiple values here until we
    ;; know we're either in a multi-valued clause (in which case we know the #
    ;; of vals) or we're not, in which case we only care about the first one.
    (let [[val] vals]
      (if (or (and (sym? pattern) ; unification with outer locals (or nil)
                   (not= :_ (tostring pattern)) ; never unify _
                   (or (in-scope? pattern)
                       (= :nil (tostring pattern))))
              (and (multi-sym? pattern)
                   (in-scope? (. (multi-sym? pattern) 1))))
          (values `(= ,val ,pattern) [])
          ;; unify a local we've seen already
          (and (sym? pattern)
               (. unifications (tostring pattern)))
          (values `(= ,(. unifications (tostring pattern)) ,val) [])
          ;; bind a fresh local
          (sym? pattern)
          (let [wildcard? (= (tostring pattern) "_")]
            (if (not wildcard?) (tset unifications (tostring pattern) val))
            (values (if (or wildcard? (: (tostring pattern) :find "^?"))
                        true `(not= ,(sym :nil) ,val))
                    [pattern val]))
          ;; guard clause
          (and (list? pattern) (sym? (. pattern 2)) (= :? (tostring (. pattern 2))))
          (let [(pcondition bindings) (match-pattern vals (. pattern 1)
                                                     unifications)
                condition `(and ,pcondition)]
            (for [i 3 (# pattern)] ; splice in guard clauses
              (table.insert condition (. pattern i)))
            (values `(let ,bindings ,condition) bindings))
  
          ;; multi-valued patterns (represented as lists)
          (list? pattern)
          (let [condition `(and)
                bindings []]
            (each [i pat (ipairs pattern)]
              (let [(subcondition subbindings) (match-pattern [(. vals i)] pat
                                                              unifications)]
                (table.insert condition subcondition)
                (each [_ b (ipairs subbindings)]
                  (table.insert bindings b))))
            (values condition bindings))
          ;; table patterns
          (= (type pattern) :table)
          (let [condition `(and (= (type ,val) :table))
                bindings []]
            (each [k pat (pairs pattern)]
              (if (and (sym? pat) (= "&" (tostring pat)))
                  (do (assert (not (. pattern (+ k 2)))
                              "expected rest argument before last parameter")
                      (table.insert bindings (. pattern (+ k 1)))
                      (table.insert bindings [`(select ,k ((or _G.unpack
                                                               table.unpack)
                                                           ,val))]))
                  (and (= :number (type k))
                       (= "&" (tostring (. pattern (- k 1)))))
                  nil ; don't process the pattern right after &; already got it
                  (let [subval `(. ,val ,k)
                        (subcondition subbindings) (match-pattern [subval] pat
                                                                  unifications)]
                    (table.insert condition subcondition)
                    (each [_ b (ipairs subbindings)]
                      (table.insert bindings b)))))
            (values condition bindings))
          ;; literal value
          (values `(= ,val ,pattern) []))))
  
  (fn match-condition [vals clauses]
    "Construct the actual `if` AST for the given match values and clauses."
    (if (not= 0 (% (length clauses) 2)) ; treat odd final clause as default
        (table.insert clauses (length clauses) (sym :_)))
    (let [out `(if)]
      (for [i 1 (length clauses) 2]
        (let [pattern (. clauses i)
              body (. clauses (+ i 1))
              (condition bindings) (match-pattern vals pattern {})]
          (table.insert out condition)
          (table.insert out `(let ,bindings ,body))))
      out))
  
  (fn match-val-syms [clauses]
    "How many multi-valued clauses are there? return a list of that many gensyms."
    (let [syms (list (gensym))]
      (for [i 1 (length clauses) 2]
        (if (list? (. clauses i))
            (each [valnum (ipairs (. clauses i))]
              (if (not (. syms valnum))
                  (tset syms valnum (gensym))))))
      syms))
  
  (fn match [val ...]
    "Perform pattern matching on val. See reference for details."
    (let [clauses [...]
          vals (match-val-syms clauses)]
      ;; protect against multiple evaluation of the value, bind against as
      ;; many values as we ever match against in the clauses.
      (list `let [vals val]
            (match-condition vals clauses))))
  
  {: -> : ->> : -?> : -?>>
   : doto : when : with-open
   : partial : lambda
   : pick-args : pick-values
   : macro : macrodebug : import-macros
   : match}
  ]===]
  local module_name = "fennel.macros"
  local _ = nil
  local function _0_()
    return mod
  end
  package.preload[module_name] = _0_
  _ = nil
  local env = specials.makeCompilerEnv(nil, compiler.scopes.compiler, {})
  local built_ins = eval(builtin_macros, {allowedGlobals = false, env = env, filename = "src/fennel/macros.fnl", moduleName = module_name, scope = compiler.scopes.compiler, useMetadata = true})
  for k, v in pairs(built_ins) do
    compiler.scopes.global.macros[k] = v
  end
  compiler.scopes.global.macros["\206\187"] = compiler.scopes.global.macros.lambda
  package.preload[module_name] = nil
end
return mod

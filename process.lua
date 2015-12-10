util = require("framework.util")
throw = require("framework.throw")
log = require("framework.log")

--[[
timestamp operations
]]

ts = { }

function ts.check_type_cmp(a, b)
	if not (a.type == "special" or b.type == "special" or a.type == b.type) then
		errfmt({}, "Cannot compare %s and %s", a:pretty_print(), b:pretty_print())
	end
end

function ts.check_type_op(a, b)
	if not (a.type == b.type) then
		errfmt({}, "Cannot perform operations on %s and %s", a:pretty_print(), b:pretty_print())
	end
end

function ts.lt(a, b)
	ts.check_type_cmp(a, b)
	return a.value < b.value
end

function ts.le(a, b)
	ts.check_type_cmp(a, b)
	return a.value <= b.value
end

function ts.eq(a, b)
	ts.check_type_cmp(a, b)
	return a.value == b.value
end

function ts.sub(a, b)
	if b == ts.make_begin() then return a end
	ts.check_type_op(a, b)
	local ret_value = a.value - b.value
	return ts.parse_type_value(a.type, ret_value)
end

function ts.parse_type_value(_type, value)
	if _type == "time" then
		local ret = { type = "time", time = { h = 0, m = 0, s = 0 }, value = 0 }
		setmetatable(ret, {
			__eq = ts.eq,
			__lt = ts.lt,
			__le = ts.le,
			__sub = ts.sub,
			__index = ts,
			__tostring = function(a)
				return string.format("%02d:%02d:%02d", a.time.h, a.time.m, a.time.s)
			end
		})

		local make_ret = function()
			ret.time.m = ret.time.m + math.floor(ret.time.s/60)
			ret.time.s = ret.time.s%60

			ret.time.h = ret.time.h + math.floor(ret.time.m/60)
			ret.time.m = ret.time.m%60

			ret.value = ret.time.h*3600 + ret.time.m*60 + ret.time.s
			return ret
		end

		if type(value) == "number" then
			ret.time.s = value
			return make_ret()
		else
			if pcall(function()                         ret.time.s = util.map_tuple(util.tonumber, value:match(            "^(%d+)$")) end) then return make_ret() end
			if pcall(function()             ret.time.m, ret.time.s = util.map_tuple(util.tonumber, value:match(      "^(%d+):(%d+)$")) end) then return make_ret() end
			if pcall(function() ret.time.h, ret.time.m, ret.time.s = util.map_tuple(util.tonumber, value:match("^(%d+):(%d+):(%d+)$")) end) then return make_ret() end
		end

		errfmt({}, "Invalid time ts value specification: '%s'", value)
	elseif _type == "frame" then
		local ret = { type = "frame", value = 0 }
		setmetatable(ret, {
			__eq = ts.eq,
			__lt = ts.lt,
			__le = ts.le,
			__sub = ts.sub,
			__index = ts,
			__tostring = function(a)
				return tostring(a.value)
			end
		})

		if type(value) == "number" then
			ret.value = value
			return ret
		else
			if pcall(function() ret.value = util.tonumber(value:match("^(%d+)$")) end) then return ret end
		end

		errfmt({}, "Invalid frame ts value specification: '%s'", value)
	end

	errfmt({}, "Invalid ts specification: type '%s' value '%s'", _type, value)
end

function ts.parse(string)
	return ts.parse_type_value(string:match("(.-):(.*)"))
end

function ts.make_begin()
	local ret = { type = "special", mode = "begin", value = 0 }
	setmetatable(ret, {
		__eq = ts.eq,
		__lt = ts.lt,
		__le = ts.le,
		__index = ts,
		__tostring = function(a)
			return "<begin>"
		end
	})
	return ret
end

function ts.make_end()
	local ret = { type = "special", mode = "end", value = math.huge }
	setmetatable(ret, {
		__eq = ts.eq,
		__lt = ts.lt,
		__le = ts.le,
		__index = ts,
		__tostring = function(a)
			return "<end>"
		end
	})
	return ret
end

function ts.pretty_print(ts)
	return string.format("%s ts %s", ts.type, tostring(ts))
end

--[[
data types
]]

-- Input filter specifications.
--      from: filter begin ts
--      to: filter end ts
--      filter: ffmpeg-formatted filter description
in_filters = { }

-- Output filter specifications graph.
--      srcs: source filter pointer(s)
--      from: filter begin ts
--      to: filter end ts
--      filter: ffmpeg-formatted filter description
-- The from and to timestamps are real. The timestamps in the stream output by the filter are counted from 0.
out_filters = { }

-- Output filter "authoritative ranges" -- topmost filters in the graph for each part of the timeline.
--      entry: pointer to out_filter (srcs, from, to, filter)
--      from: authoritative range begin ts
--      to: authoritative range end ts
out_filters_authoritative = { }
setmetatable(out_filters_authoritative, {
	__index = function(table, key)
		-- key is a ts
		if not util.is_table(key) or not key.type or not key.value then
			return nil
		end

		local index, candidate

		for k, v in pairs(table) do
			if v.from <= key and key < v.to then
			   	if not candidate then
					index, candidate = k, v
				else
					errfmt("multiple authoritative filters for %s: first is '%s', second is '%s'",
					       key:pretty_print(), table.concat(candidate.entry.filter.args, " "), table.concat(v.entry.filter.args, " "))
				end
			end
		end

		return { index, candidate }
	end
})

-- Initialize the first "filter": the source.
-- (FIXME: support audio sources)
out_filters[1] = { srcs = { }, id = "0:v", from = ts.make_begin(), to = ts.make_end() }
out_filters_authoritative[1] = { entry = out_filters[1], from = ts.make_begin(), to = ts.make_end() }

--[[
argument parsing
]]

function take_next_arg(index, arguments)
	local ret_i, ret_a = index+1, arguments[index+1]
	table.remove(arguments, ret_i)
	return ret_i, ret_a
end

function parse_filter(args)
	local ret = { args = util.clone(args), from = ts.make_begin(), to = ts.make_end() }

	for i, a in ipairs(args) do
		if a == "-s" or a == "--start" then
			i, a = take_next_arg(i, args)
			ret.from = ts.parse(a)
		elseif a == "-e" or a == "--end" then
			i, a = take_next_arg(i, args)
			ret.to = ts.parse(a)
		elseif a == "-f" or a == "--filter" then
			i, a = take_next_arg(i, args)
			ret.filter = a
		else
			errfmt({}, "Invalid argument: '%s'", a)
		end
	end

	return ret
end

table.insert(arg, "--")
current_filter = { }
for i, a in ipairs(arg) do
	if a == "--" and #current_filter > 0 then
		throw.try(function()
			table.insert(in_filters, parse_filter(current_filter))
		end, function(e)
			errfmt({}, "Invalid filter specification '%s': %s", table.concat(current_filter, " "), tostring(e))
		end)
		current_filter = { }
	else
		table.insert(current_filter, a)
	end
end
current_filter = nil

--[[
ancillary filter generators
]]

id_counter = 1
function generate_out_id()
	local ret = string.format("id_%d", id_counter)
	id_counter = id_counter + 1
	return ret
end

function build_trim_for_entry_and_range(entry, from, to)
	assertfmt(entry.from <= from, "Cannot trim entry [%s; %s) to start at %s", entry.from:pretty_print(), entry.to:pretty_print(), from:pretty_print())
	assertfmt(to <= entry.to, "Cannot trim entry [%s; %s) to end at %s", entry.from:pretty_print(), entry.to:pretty_print(), to:pretty_print())

	-- check if we need to trim the range at all
	if(entry.from == from and entry.to == to) then
		return entry
	end

	-- pick proper keywords for trim's start and end based on ts type
	local trim_opt_start, trim_opt_end
	if from.type == "time" or to.type == "time" then
		trim_opt_start = "start"
		trim_opt_end = "end"
	elseif from.type == "frame" or to.type == "frame" then
		trim_opt_start = "start_frame"
		trim_opt_end = "end_frame"
	elseif from.type == "special" and to.type == "special" then
		-- both are special? this should've been dealt with first two assertions and early-return
		errfmt({}, "Will not do trim of [%s; %s) to [%s; %s)", entry.from:pretty_print(), entry.to:pretty_print(), from.pretty_print(), to:pretty_print())
	else
		errfmt({}, "Unrecognized ts types: trimming to [%s; %s)", from.pretty_print(), to:pretty_print())
	end

	-- generate the trim filter string
	local trim_opts = { }

	if (entry.from < from) then
		table.insert(trim_opts, string.format("%s=%d", trim_opt_start, from.value - entry.from.value))
	end

	if (to < entry.to) then
		table.insert(trim_opts, string.format("%s=%d", trim_opt_end, to.value - entry.from.value))
	end

	-- the filter entry template
	local trim_entry = {
		srcs = { entry },
		from = from,
		to = to,

		-- build the filter string
		-- if needed, retimestamp the filter's output for usage in further concat and for uniformity
		filter = "trim=" .. table.concat(trim_opts, ":") .. (entry.from < from and ",setpts=PTS-STARTPTS" or "")
	}

	-- save the output and return
	table.insert(out_filters, trim_entry)
	return trim_entry
end

function build_concat_for_entries(entries)
	assert(#entries > 0, "Cannot build concatenating filter for 0 entries")
	if #entries == 1 then return entries[1] end

	local concat_entry = {
		srcs = entries,
		from = entries[1].from,
		to = entries[#entries].to,

		-- build the filter string (FIXME: support audio?)
		filter = "concat=" .. string.format("n=%d:v=1:a=0", #entries)
	}

	-- save the output and return
	table.insert(out_filters, concat_entry)
	return concat_entry
end

function build_input_for_range(from, to)
	local entries = {  }
	local done_up_to = from
	repeat
		local i, range = unpack(out_filters_authoritative[done_up_to])
		assertfmt(range, "no authoritative filter for %s", done_up_to:pretty_print())
		
		-- The authoritative range we found is [ts1; ts2). We want range [done_up_to; to).
		-- Nine situations: { ts1 <=> done_up_to } * { ts2 <=> to }.
		-- Situations where `ts1 > done_up_to` are impossible because we look up the entry by `done_up_to`.

		if     range.from == done_up_to and range.to <= to then -- we take over whole range
			-- add appropriately trimmed filter to our list
			table.insert(entries, build_trim_for_entry_and_range(range.entry, range.from, range.to))

			-- remove the authoritative range
			table.remove(out_filters_authoritative, i)

			-- advance up to the range's end
			done_up_to = range.to
		elseif range.from <  done_up_to and range.to <= to then -- we take over the range's tail [done_up_to; range.to)
			-- add appropriately trimmed filter to our list
			table.insert(entries, build_trim_for_entry_and_range(range.entry, done_up_to, range.to))

			-- this branch is possible only on first iteration
			-- (otherwise the head of the range would be already taken by us)
			assert(done_up_to == from)

			-- advance up to the range's end
			done_up_to = range.to

			-- cut authoritative range's tail
			range.to = from
		elseif range.from == done_up_to and range.to  > to then
			-- add appropriately trimmed filter to our list
			table.insert(entries, build_trim_for_entry_and_range(range.entry, range.from, to))

			-- cut authoritative range's head
			range.from = to

			-- advance to the end
			done_up_to = to
		elseif range.from <  done_up_to and range.to  > to then
			-- add appropriately trimmed filter to our list
			table.insert(entries, build_trim_for_entry_and_range(range.entry, done_up_to, to))

			-- this branch is possible only on first iteration
			-- (otherwise the head of the range would be already taken by us)
			assert(done_up_to == from)

			-- split the authoritative range in two
			table.remove(out_filters_authoritative, i)
			table.insert(out_filters_authoritative, {
				entry = range.entry,
				from = range.from,
				to = from
			})
			table.insert(out_filters_authoritative, {
				entry = range.entry,
				from = to,
				to = range.to
			})


			-- advance to the end
			done_up_to = to
		else
			throw.error("Logic error")
		end
	until done_up_to == to

	-- now concatenate built pieces, but don't make that filter authoritative (the caller knows better:
	-- our output will most probably be used as an input for another filter)
	local result = build_concat_for_entries(entries)
	assert(result.from == from)
	assert(result.to == to)
	return result
end

function apply_filter_for_range(filter, from, to)
	local filter_entry = {
		srcs = { build_input_for_range(from, to) },
		from = from,
		to = to,
		filter = filter
	}

	table.insert(out_filters, filter_entry)
	table.insert(out_filters_authoritative, {
		entry = filter_entry,
		from = from,
		to = to
	})
end

function render_filter_subtree(entry)
	assert(util.is_table(entry.srcs))
	assert(util.is_string(entry.filter))

	local entry_srcs_nr = #entry.srcs

	if entry_srcs_nr < 1 then
		return entry.filter
	elseif entry_srcs_nr == 1 then
		local e = entry.srcs[1]
		if e.filter then
			return render_filter_subtree(e) .. "," .. entry.filter
		elseif e.id then
			return string.format("[%s]%s", e.id, entry.filter)
		else
			error("A filter entry has neither a filter string nor an opaque label (for source filters)")
		end
	else
		local srcs_rendered = { }
		local srcs_labels = { }

		for i, e in ipairs(entry.srcs) do
			local e_rendered, e_label
			if e.filter then
				e_label = string.format("[%s]", generate_out_id())
				table.insert(srcs_rendered, render_filter_subtree(e) .. e_label)
			elseif e.id then
				e_label = e.id
			else
				error("A filter entry has neither a filter string nor an opaque label (for source filters)")
			end
			table.insert(srcs_labels, e_label)
		end

		table.insert(srcs_rendered, table.concat(srcs_labels, "") .. entry.filter)
		return table.concat(srcs_rendered, ";")
	end
end

for _, f in ipairs(in_filters) do
	apply_filter_for_range(f.filter, f.from, f.to)
end

result = build_input_for_range(ts.make_begin(), ts.make_end())

print(render_filter_subtree(result))

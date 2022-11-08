module UHDAPI
using UHDBindings.LibUHD
using Match

## General helper functions
export uhd_error;
abstract type Uhdobject end

function getstr(f::Function) :: String
    n=100;
    while true
        s=zeros(Cuchar,n);
        f(s,length(s));
        if s[end]==0
            return GC.@preserve s unsafe_string(pointer(s));
        end
        n=2n;
    end
end

function getvalue(f::Function, t::Type{T}) :: T where {T<:Uhdobject}
    tmp=t();
    f(tmp);
    return tmp;
end

function getvalue(f::Function, _::Type{String}) :: String
    getstr(f);
end

function getvalue(f::Function, _::Type{Vector{String}}) :: Vector{String}
    sv=Uhd_string_vector();
    f(sv);
    return identity.(sv);
end

function getvalue(f::Function, t::Type;convert::Type{T}=t) :: T where {T}
    tmp=Ref{t}();
    f(tmp);
    return tmp[];
end;

getvalue(f::Function,t::Type{uhd_sensor_value_data_type_t})=getvalue(f,t,convert=Type);

function getvalue(f::Function, ts::Tuple{Vararg{Type}})
    refs=map(ts) do t
        Ref{t}();
    end
    f(refs...);
    return getindex.(refs);
end

function genuhdconstructor(s::String,args::Tuple{Symbol,Type}...;f::Symbol=getuhdsym("_make",s))
    constructorargs=map(x-> Expr(:(::), x...), args);
    handle=getuhdsym("_handle",s);
    free=getuhdsym("_free",s);
    name=Symbol(uppercasefirst(s));
    isstreamer=occursin("streamer",s);
    @eval begin
        function $name($(constructorargs...))
            handle=Ref{$handle}(0);
            @checkuhderr $f(handle,$(first.(args)...));
            $(isstreamer ?
                :(x=$name(handle,Ref{Uhdobject}());) :
                :(x=$name(handle);)
            )
            finalizer(x) do xx
                #ccall(:jl_safe_printf, Cvoid, (Cstring,), "Finalizing $($name).\n"); # Debug
                @checkuhderr $free(xx);
            end;
        end;
    end
end

T=Union{Dict{Symbol,Tuple{Symbol,Type}},Dict{Symbol,Tuple{Symbol,Tuple{Vararg{Type}}}}};

function genuhdobj(s :: String,args :: Tuple{Symbol,Type}...;getters=Dict{Symbol,Tuple{Symbol,Type}}()::T)
    handle=getuhdsym("_handle",s);
    free=getuhdsym("_free",s);
    make=getuhdsym("_make",s);
    pp=getuhdsym("_to_pp_string",s);
    if isnothing(pp) pp=getuhdsym("_get_pp_string",s); end
    name=Symbol(uppercasefirst(s));
    if any(isnothing.([handle,free,make]))
        error(string("$s is not a valid part of the UHD API."));
    end
    isstreamer=occursin("streamer",s);
    @eval begin
        export $name;
        mutable struct $(name) <: Uhdobject
            handle :: Ref{$handle}
            $(isstreamer ? :(usrp :: Ref{T} where {T<:Uhdobject}) : nothing)
        end

        function Base.getproperty(obj::$name , sym::Symbol)
            g=$getters;
            if sym===:h
                h=getfield(obj,:handle);
            if h[]==C_NULL
                    error(string("Attempting to access uninitialized $name"));
                end
                return h;
            end
            @match sym begin
                $([ :($(Meta.quot(k)) => return getvalue((xx...) -> @checkuhderr($(getters[k][1])(obj,xx...)),$(getters[k][2])) ) for k in keys(getters)]...)
                _ => return getfield(obj,sym);
            end
        end

        Base.propertynames(t::Type{$name},private::Bool=false)=$(tuple(:h,keys(getters)...));
	    Base.cconvert(_::Type{$handle}, x::$name) = x;
        Base.cconvert(_::Type{Ptr{$handle}}, x::$name) = x;
	    Base.unsafe_convert(_::Type{$handle}, x::$name) = x.h[];
        Base.unsafe_convert(t::Type{Ptr{$handle}}, x::$name) = Base.unsafe_convert(t,x.h);
    end
    if !isnothing(pp)
        @eval function Base.show(io::IO,x::$name)
            write(io,getstr() do s,l
                @checkuhderr $pp(x,s,l);
            end);
        end
    end
    genuhdconstructor(s,args...,f=make);
end;

function genuhdget(name::Symbol,rettype::Union{Type,Tuple{Vararg{Type}}},params::Tuple{Symbol,Type}...;prefix::Symbol=:uhd_usrp_,t::Type=Uhd_usrp)
    @eval export $name;
    @eval function $name(obj::$t,$(map(x-> Expr(:(::), x...), params)...))
        getvalue($rettype) do x...
            @checkuhderr $(Symbol(prefix,name))(obj,$(first.(params)...),x...)
        end
    end;
end;

function genuhdset(name::Symbol,params::Tuple{Symbol,Type}...;prefix::Symbol=:uhd_usrp_,t::Type=Uhd_usrp)
    @eval export $name;
    @eval function $name(obj::$t,$(map(x-> Expr(:(::), x...), params)...))
        @checkuhderr $(Expr(:call,Symbol(prefix,name),:obj,first.(params)...));
    end;
end;

function getuhdsym(suffix :: String, ex :: Union{Symbol,String} )
    m=names(LibUHD);
    filter!(endswith(suffix)∘String,m);
    sort!(m,by=length∘String,rev=true);
    ms=chop.(String.(m),tail=length(suffix));
    i=findfirst(x->startswith(String(ex),x),ms);
    return isnothing(i) ? nothing : m[i];
end;

macro checkuhderr(ex)
    if isa(ex,Expr) && ex.head===:call
        fs=ex.args[1]; # Which function is being called?
        h=ex.args[2]; # What is it’s first argument?
        # make and free have a global get_last_error function
        if endswith(string(fs),"_make") || endswith(string(fs),"_free")
            geterr=:uhd_get_last_error;
            globalerr=true;
        else # The other functions have object specific functions.
            geterr=getuhdsym("_last_error",fs);
            globalerr=false;
        end
        if !isnothing(geterr)
            return quote
                uhd_errcode=$(esc(ex));
                if uhd_errcode!==UHD_ERROR_NONE
                    errmsg=getstr() do s,l
                        $(globalerr ?
                        :($geterr(s,l)) : # The global last_error function does not take a handle
                        :($geterr($(esc(h)),s,l)));
                    end
                    #error(string($fs," returned: ",repr(uhd_errcode),"\n",errmsg));
                    error("$($fs) returned $(repr(uhd_errcode)):\n $errmsg");
                end
                uhd_errcode
            end
        end
    end
    @warn "This does not seem to be a uhd function."
    return esc(ex);    
end;

# Part of the UHD API is just wrappers around C++ vectors.
# This generates wrappers to access them in a more convenient way.
function genvectorhelpers(s::String,t::Type)
    sz=getuhdsym("_size",s);
    p=getuhdsym("_push_back",s);
    at=getuhdsym("_at",s);
    n=Symbol(uppercasefirst(s));
    if any(isnothing.([s,p,at]))
        error(string(s," is not a valid part of the UHD API."));
    end
    @eval begin
        function Base.length(x :: $n) :: Int
            getvalue(Csize_t) do len 
                @checkuhderr($sz(x,len));
            end
        end;
        function Base.push!(x :: $n, data::$t...)
            for xx in data
                @checkuhderr($p(x,xx));
            end
        end;
        function Base.getindex(xx::$n, i::Int) ::$t
            getvalue($t) do x...
                @checkuhderr($at(xx,i-1,x...));
            end
        end
        function Base.iterate(x :: $n, i::Int=1)
            if i>length(x)
                return nothing;
            end
            return (x[i],i+1);
        end;
    end
end;



## Uhd_string_vector

genuhdobj("uhd_string_vector");
genvectorhelpers("uhd_string_vector",String);

## Uhd_dboard_eeprom
begin
    local g=Dict(
        :id => (:uhd_dboard_eeprom_get_id,String),
        :serial => (:uhd_dboard_eeprom_get_serial,String),
        :revision => (:uhd_dboard_eeprom_get_revision,Cint),
    )
    genuhdobj("uhd_dboard_eeprom",getters=g);
end

function Base.setproperty!(ee::Uhd_dboard_eeprom, name::Symbol ,val)
    @match name begin
        :id => @checkuhderr uhd_dboard_eeprom_set_id(ee,val)
        :serial => @checkuhderr uhd_dboard_eeprom_set_serial(ee,val)
        :revision => @checkuhderr uhd_dboard_eeprom_set_revision(ee,val)
        _ => return setfield!(ee,sym,val);
    end
    return val;
end

## Uhd_mboard_eeprom
genuhdobj("uhd_mboard_eeprom");

function Base.getindex(ee::Uhd_mboard_eeprom,key::String) :: String
    getstr() do s,l
        @checkuhderr uhd_mboard_eeprom_get_value(ee,key,s,l);
    end
end

function Base.setindex!(ee::Uhd_mboard_eeprom,value::String,key::String)
    @checkuhderr uhd_mboard_eeprom_set_value(ee,key,value);
end

## Uhd_async_metadata
export uhd_async_metadata_event_code_t;
for name in names(LibUHD; all=true)
    if startswith(string(name), "UHD_ASYNC_METADATA_EVENT_CODE")
        @eval export $name
    end
end
begin
    local g=Dict(
        :channel => (:uhd_async_metadata_channel,Csize_t),
        :has_time_spec => (:uhd_async_metadata_has_time_spec,Bool),
        :time_spec => (:uhd_async_metadata_time_spec,(Int64,Cdouble)),
        :event_code => (:uhd_async_metadata_event_code,uhd_async_metadata_event_code_t),
        :user_payload => (:uhd_async_metadata_user_payload,UInt32)
    );
    genuhdobj("uhd_async_metadata",getters=g);
end

## uhd_rx_metadata
export uhd_rx_metadata_error_code_t;
for name in names(LibUHD; all=true)
    if startswith(string(name), "UHD_RX_METADATA_ERROR_CODE")
        @eval export $name
    end
end
begin
    local g=Dict(
        :has_time_spec => (:uhd_rx_metadata_has_time_spec,Bool),
        :time_spec => (:uhd_rx_metadata_time_spec,(Int64,Cdouble)),
        :more_fragments => (:uhd_rx_metadata_more_fragments,Bool),
        :fragment_offset => (:uhd_rx_metadata_fragment_offset,Csize_t),
        :start_of_burst => (:uhd_rx_metadata_start_of_burst,Bool),
        :end_of_burst => (:uhd_rx_metadata_end_of_burst,Bool),
        :out_of_sequence => (:uhd_rx_metadata_out_of_sequence,Bool),
        :error_code => (:uhd_rx_metadata_error_code,uhd_rx_metadata_error_code_t),
        :strerror => (:uhd_rx_metadata_strerror,String)
    );
    genuhdobj("uhd_rx_metadata",getters=g);
end

## uhd_tx_metadata
begin
    local g=Dict(
        :has_time_spec => (:uhd_tx_metadata_has_time_spec,Bool),
        :time_spec => (:uhd_tx_metadata_time_spec,(Int64,Cdouble)),
        :start_of_burst => (:uhd_tx_metadata_start_of_burst,Bool),
        :end_of_burst => (:uhd_tx_metadata_end_of_burst,Bool),
    );
    genuhdobj("uhd_tx_metadata",(:has_time_spec,Bool), (:full_secs,Int64), (:frac_secs,Cdouble), (:start_of_burst,Bool), (:end_of_burst,Bool),getters=g);
end

## uhd_rx_streamer
export uhd_stream_cmd_t, uhd_stream_mode_t;
for name in names(LibUHD; all=true)
    if startswith(string(name), "UHD_STREAM_MODE")
        @eval export $name
    end
end
begin
    local g=Dict(
        :num_channels => (:uhd_rx_streamer_num_channels,Csize_t),
        :max_num_samps => (:uhd_rx_streamer_max_num_samps,Csize_t)
    );
    genuhdobj("uhd_rx_streamer",getters=g);
end
export recv!;
function recv!(buffers::Vector{T},streamer::Uhd_rx_streamer,samps_per_buff::Csize_t,timeout::Cdouble,one_packet::Bool=false) :: Tuple{Csize_t,Uhd_rx_metadata} where {T<:Ref}
    md=Uhd_rx_metadata();
    pointers=pointer.(getindex.(buffers));
    GC.@preserve buffers items_received=getvalue(Csize_t) do items_received
        @checkuhderr uhd_rx_streamer_recv(streamer,pointers,samps_per_buff,md,timeout,one_packet,items_received);
    end
    return (items_received,md);
end
genuhdset(:issue_stream_cmd,(:stream_cmd,uhd_stream_cmd_t),prefix=:uhd_rx_streamer_,t=Uhd_rx_streamer)

Base.cconvert(_::Type{Ptr{uhd_stream_cmd_t}},x::uhd_stream_cmd_t) = Ref(x);

## uhd_tx_streamer
begin
    local g=Dict(
        :num_channels => (:uhd_tx_streamer_num_channels,Csize_t),
        :max_num_samps => (:uhd_tx_streamer_max_num_samps,Csize_t)
    );
    genuhdobj("uhd_tx_streamer",getters=g);
end

export recv_async_msg;
function recv_async_msg(streamer::Uhd_tx_streamer,timeout::Cdouble) :: Union{Nothing,Uhd_async_metadata}
    md=Uhd_async_metadata();
    valid=Ref{Bool}();
    @checkuhderr uhd_tx_streamer_recv_async_msg(streamer,md.h,timeout,valid);
    return valid[] ? md : nothing;
end

export send;
function send(streamer::Uhd_tx_streamer,buffers::Vector{T},samps_per_buff::Csize_t,md::Uhd_tx_metadata,timeout::Cdouble) :: Csize_t where {T<:Ref}
    pointers=pointer.(getindex.(buffers));
    GC.@preserve buffers items_received=getvalue(Csize_t) do items_sent
        @checkuhderr uhd_tx_streamer_send(streamer,pointers,samps_per_buff,md,timeout,items_sent);
    end
end



## Uhd_meta_range
export uhd_range_t;
begin
    local g=Dict(
        :start => (:uhd_meta_range_start,Cdouble),
        :stop => (:uhd_meta_range_stop,Cdouble),
        :step => (:uhd_meta_range_step,Cdouble)
    );
    genuhdobj("uhd_meta_range",getters=g);
    genvectorhelpers("uhd_meta_range",uhd_range_t);
end

Base.cconvert(_::Type{Ptr{uhd_range_t}},x::uhd_range_t) = Ref{uhd_range_t}(x);
genuhdget(:clip,Cdouble,(:value,Cdouble),(:clip_step,Bool),prefix=:uhd_meta_range,t=Uhd_meta_range);

## uhd_stream_args_t
export Uhd_stream_args;
struct Uhd_stream_args
    cpu_format :: String
    otw_format :: String
    args :: String
    channel_list :: Vector{UInt64}
end

function Base.getproperty(obj::Uhd_stream_args,sym::Symbol)
    if sym===:cref
        return Ref(uhd_stream_args_t(
            Base.unsafe_convert(Cstring,obj.cpu_format),
            Base.unsafe_convert(Cstring,obj.otw_format),
            Base.unsafe_convert(Cstring,obj.args),
            pointer(obj.channel_list),
            length(obj.channel_list)
        ));
    else
        return getfield(obj,sym);
    end
end

## uhd_tune_request_t
export Uhd_tune_request;
struct Uhd_tune_request
    target_freq :: Float64
    rf_freq_policy :: uhd_tune_request_policy_t
    rf_freq :: Float64
    dsp_freq_policy :: uhd_tune_request_policy_t
    dsp_freq :: Float64
    args :: String
end

function Base.getproperty(obj::Uhd_tune_request,sym::Symbol)
    if sym===:cref
        return Ref(uhd_tune_request_t(
            obj.target_freq,
            obj.rf_freq_policy,
            obj.rf_freq,
            obj.dsp_freq_policy,
            obj.dsp_freq,
            Base.unsafe_convert(Cstring,obj.args)
        ));
    else
        return getfield(obj,sym);
    end
end

function Uhd_tune_request(freq::Float64) :: Uhd_tune_request
    Uhd_tune_request(freq,UHD_TUNE_REQUEST_POLICY_AUTO,0.0,UHD_TUNE_REQUEST_POLICY_AUTO,0.0,"");
end
function Uhd_tune_request(freq::Float64,lo_off::Float64) :: Uhd_tune_request
    Uhd_tune_request(freq,UHD_TUNE_REQUEST_POLICY_MANUAL,freq+lo_off,UHD_TUNE_REQUEST_POLICY_AUTO,0.0,"");
end

## uhd_usrp_rx_info_t
export Uhd_usrp_rx_info
@eval struct Uhd_usrp_rx_info
    $([:($n :: String) for n in fieldnames(uhd_usrp_rx_info_t)]...)
end

## uhd_usrp_tx_info_t
export Uhd_usrp_rx_info
@eval struct Uhd_usrp_tx_info
    $([:($n :: String) for n in fieldnames(uhd_usrp_tx_info_t)]...)
end


## Uhd_sensor_value
begin
    g=Dict(
        :name => (:uhd_sensor_value_name,String),
        :value => (:uhd_sensor_value_value,String),
        :unit => (:uhd_sensor_value_unit,String),
        :data_type => (:uhd_sensor_value_data_type,uhd_sensor_value_data_type_t),
    );
    genuhdobj("uhd_sensor_value",getters=g);
    genuhdconstructor("uhd_sensor_value",(:name,String),(:value,Bool),(:utrue,String),(:ufalse,String),f=:uhd_sensor_value_make_from_bool);
    genuhdconstructor("uhd_sensor_value",(:name,String),(:value,Int),(:unit,String),(:formatter,String),f=:uhd_sensor_value_make_from_int);
    genuhdconstructor("uhd_sensor_value",(:name,String),(:value,Cdouble),(:unit,String),(:formatter,String),f=:uhd_sensor_value_make_from_realnum);
    genuhdconstructor("uhd_sensor_value",(:name,String),(:value,String),(:unit,String),f=:uhd_sensor_value_make_from_string);
    genuhdget(:to_int,Int,prefix=:uhd_sensor_value_,t=Uhd_sensor_value);
    genuhdget(:to_bool,Bool,prefix=:uhd_sensor_value_,t=Uhd_sensor_value);
    genuhdget(:to_realnum,Cdouble,prefix=:uhd_sensor_value_,t=Uhd_sensor_value);
end

function Base.convert(_::Type{Type},t::uhd_sensor_value_data_type_t)
    if t≡UHD_SENSOR_VALUE_BOOLEAN Bool
    elseif t≡UHD_SENSOR_VALUE_INTEGER Int
    elseif t≡UHD_SENSOR_VALUE_REALNUM Cdouble
    elseif t≡UHD_SENSOR_VALUE_STRING String
    else error("Invalid value for uhd_sensor_value_data_type.");
    end
end

## Uhd_subdev_spec
genuhdobj("uhd_subdev_spec",(:name,String));
Uhd_subdev_spec()=Uhd_subdev_spec("");
#TODO

## Find functions
export usrp_find;
function usrp_find(args :: String="") :: Vector{String}
    getvalue(Vector{String}) do x
       uhd_usrp_find(args,x);
    end
end;

export usrp_clock_find;
function usrp_clock_find(args :: String="") :: Vector{String}
    getvalue(Vector{String}) do x
       uhd_usrp_clock_find(args,x);
    end
end;

## Simple string getter functions.
export abi_string, version_string;
abi_string() = getstr(uhd_get_abi_string);
version_string() = getstr(uhd_get_version_string);

## Uhd_usrp
genuhdobj("uhd_usrp",(:args,String));
Uhd_usrp()=Uhd_usrp("");
export set_rx_freq, set_tx_freq, get_rx_stream, get_tx_stream, get_rx_info, get_tx_info, get_mboard_eeprom, get_dboard_eeprom;
function set_rx_freq(u::Uhd_usrp,tune_request::Uhd_tune_request,chan::Csize_t) :: uhd_tune_result_t
    GC.@preserve tune_request getvalue(uhd_tune_result_t) do tune_result
        @checkuhderr uhd_usrp_set_rx_freq(u,tune_request.cref,chan,tune_result);
    end
end

function set_tx_freq(u::Uhd_usrp,tune_request::Uhd_tune_request,chan::Csize_t) :: uhd_tune_result_t
    GC.@preserve tune_request getvalue(uhd_tune_result_t) do tune_result
        @checkuhderr uhd_usrp_set_tx_freq(u,tune_request.cref,chan,tune_result);
    end
end

function get_rx_stream(u::Uhd_usrp,args::Uhd_stream_args) :: Uhd_rx_streamer
    rxs=GC.@preserve args getvalue(Uhd_rx_streamer) do x
        @checkuhderr uhd_usrp_get_rx_stream(u,args.cref,x);
    end
    rxs.usrp=Ref(u); # Prevents the USRP object from being garbage collected while the streamer exists.
    return rxs;
end

function get_tx_stream(u::Uhd_usrp,args::Uhd_stream_args) :: Uhd_tx_streamer
    txs=GC.@preserve args getvalue(Uhd_tx_streamer) do x
        @checkuhderr uhd_usrp_get_tx_stream(u,args.cref,x);
    end
    txs.usrp=Ref(u); # Prevents the USRP object from being garbage collected while the streamer exists.
    return txs;
end

function get_rx_info(u::Uhd_usrp,chan::Int) :: Uhd_usrp_rx_info
    tmp=getvalue(uhd_usrp_rx_info_t) do x
        @checkuhderr uhd_usrp_get_rx_info(u,chan,x);
    end
    s=[unsafe_string(getfield(tmp,k)) for k in fieldnames(uhd_usrp_rx_info_t)];
    @checkuhderr uhd_usrp_rx_info_free(Ref(tmp));
    info=Uhd_usrp_rx_info(s...);
end

function get_tx_info(u::Uhd_usrp,chan::Int) :: Uhd_usrp_tx_info
    tmp=getvalue(uhd_usrp_tx_info_t) do x
        @checkuhderr uhd_usrp_get_tx_info(u,chan,x);
    end
    s=[unsafe_string(getfield(tmp,k)) for k in fieldnames(uhd_usrp_tx_info_t)];
    @checkuhderr uhd_usrp_tx_info_free(Ref(tmp));
    info=Uhd_usrp_tx_info(s...);   
end

function get_mboard_eeprom(u::Uhd_usrp,mboard::Int) :: Uhd_mboard_eeprom
    getvalue(Uhd_mboard_eeprom) do x
        @checkuhderr uhd_usrp_get_mboard_eeprom(u,x,mboard);
    end
end

function get_dboard_eeprom(u::Uhd_usrp, unit::String, slot::String, mboard::Int)
    getvalue(Uhd_dboard_eeprom) do x
        @checkuhderr uhd_usrp_get_dboard_eeprom(u,x,unit,slot,mboard);
    end
end


begin
    chan=(:chan,Csize_t);
    mb=(:mboard,Int);
    name=(:name,String);

    genuhdget(:get_master_clock_rate,Cdouble,mb);
    genuhdget(:get_mboard_name,String,mb);
    genuhdget(:get_time_now,(Int64,Cdouble),mb);
    genuhdget(:get_time_last_pps,(Int64,Cdouble),mb);
    genuhdget(:get_time_synchronized,Bool);
    genuhdget(:get_time_source,String,mb);
    genuhdget(:get_time_sources,Vector{String},mb);
    genuhdget(:get_clock_source,String,mb);
    genuhdget(:get_clock_sources,Vector{String},mb);
    genuhdget(:get_mboard_sensor,Uhd_sensor_value,name,mb);
    genuhdget(:get_num_mboards,Csize_t);
    genuhdget(:get_mboard_sensor_names,Vector{String},mb);
    genuhdget(:get_rx_subdev_spec,Uhd_subdev_spec,mb);
    genuhdget(:get_rx_num_channels,Csize_t);
    genuhdget(:get_rx_subdev_name,String,chan);
    genuhdget(:get_rx_rate,Cdouble,chan);
    genuhdget(:get_rx_rates,Uhd_meta_range,chan);
    genuhdget(:get_rx_freq,Cdouble,chan);
    genuhdget(:get_rx_freq_range,Uhd_meta_range,chan);
    genuhdget(:get_fe_rx_freq_range,Uhd_meta_range,chan);
    genuhdget(:get_rx_lo_names,Vector{String},chan);
    genuhdget(:get_rx_lo_source,String,name,chan);
    genuhdget(:get_rx_lo_sources,Vector{String},name,chan);
    genuhdget(:get_rx_lo_export_enabled,Bool,name,chan);
    genuhdget(:get_rx_lo_freq,Cdouble,name,chan);
    genuhdget(:get_rx_gain,Cdouble,chan,(:gain_name,String));
    genuhdget(:get_normalized_rx_gain,Cdouble,chan);
    genuhdget(:get_rx_gain_range,Uhd_meta_range,name,chan);
    genuhdget(:get_rx_gain_names,Vector{String},chan);
    genuhdget(:get_rx_antenna,String,chan);
    genuhdget(:get_rx_antennas,Vector{String},chan);
    genuhdget(:get_rx_sensor_names,Vector{String},chan);
    genuhdget(:get_rx_bandwidth,Cdouble,chan);
    genuhdget(:get_rx_bandwidth_range,Uhd_meta_range,chan);
    genuhdget(:get_rx_sensor,Uhd_sensor_value,name,chan);
    genuhdget(:get_tx_subdev_spec,Uhd_subdev_spec,mb);
    genuhdget(:get_tx_num_channels,Csize_t);
    genuhdget(:get_tx_subdev_name,String,chan);
    genuhdget(:get_tx_rate,Cdouble,chan);
    genuhdget(:get_tx_rates,Uhd_meta_range,chan);
    genuhdget(:get_tx_freq,Cdouble,chan);
    genuhdget(:get_tx_freq_range,Uhd_meta_range,chan);
    genuhdget(:get_fe_tx_freq_range,Uhd_meta_range,chan);
    genuhdget(:get_tx_lo_names,Vector{String},chan);
    genuhdget(:get_tx_lo_source,String,name,chan);
    genuhdget(:get_tx_lo_sources,Vector{String},name,chan);
    genuhdget(:get_tx_lo_export_enabled,Bool,name,chan);
    genuhdget(:get_tx_lo_freq,Cdouble,name,chan);
    genuhdget(:get_tx_gain_range,Uhd_meta_range,name,chan);
    genuhdget(:get_tx_gain,Cdouble,chan);
    genuhdget(:get_normalized_tx_gain,Cdouble,chan);
    genuhdget(:get_tx_gain_names,Vector{String},chan);
    genuhdget(:get_tx_antenna,String,chan);
    genuhdget(:get_tx_antennas,Vector{String},chan);
    genuhdget(:get_tx_bandwidth,Cdouble,chan);
    genuhdget(:get_tx_bandwidth_range,Uhd_meta_range,chan);
    genuhdget(:get_tx_sensor,Uhd_sensor_value,name,chan);
    genuhdget(:get_tx_sensor_names,Vector{String},chan);
    genuhdget(:get_gpio_banks,Vector{String},mb);
    genuhdget(:get_gpio_attr,UInt32,(:bank,String),(:attr,String),chan);

    genuhdset(:set_master_clock_rate,(:rate,Cdouble),mb);
    genuhdset(:set_time_now,(:full_secs,Int64),(:frac_secs,Cdouble),mb);
    genuhdset(:set_time_next_pps,(:full_secs,Int64),(:frac_secs,Cdouble),mb);
    genuhdset(:set_time_unknown_pps,(:full_secs,Int64),(:frac_secs,Cdouble));
    genuhdset(:set_command_time,(:full_secs,Int64),(:frac_secs,Cdouble),mb);
    genuhdset(:clear_command_time,mb);
    genuhdset(:set_time_source,(:time_source,String),mb);
    genuhdset(:set_clock_source,(:clock_source,String),mb);
    genuhdset(:set_clock_source_out,(:enb,Bool),mb);
    genuhdset(:set_time_source_out,(:enb,Bool),mb);
    genuhdset(:set_user_register,(:addr,UInt8),(:data,UInt32),mb);
    genuhdset(:set_mboard_eeprom,(:mb_eeprom,Uhd_mboard_eeprom),mb);
    genuhdset(:set_dboard_eeprom,(:db_eeprom,Uhd_dboard_eeprom),(:unit,String),(:slot,String),mb);
    genuhdset(:set_rx_subdev_spec,(:subdev_spec,Uhd_subdev_spec),mb);
    genuhdset(:set_rx_rate,(:rate,Cdouble),chan);
    genuhdset(:set_rx_lo_source,(:src,String),name,chan);
    genuhdset(:set_rx_lo_export_enabled,(:enabled,Bool),name,chan);
    genuhdget(:set_rx_lo_freq,Cdouble,(:freq,Cdouble),name,chan);
    genuhdset(:set_rx_gain,(:gain,Cdouble),chan,(:gain_name,String));
    genuhdset(:set_normalized_rx_gain,(:gain,Cdouble),chan);
    genuhdset(:set_rx_agc,(:enable,Bool),chan);
    genuhdset(:set_rx_antenna,(:ant,String),chan);
    genuhdset(:set_rx_bandwidth,(:bandwidth,Cdouble),chan);
    genuhdset(:set_rx_dc_offset_enabled,(:enb,Bool),chan);
    genuhdset(:set_rx_iq_balance_enabled,(:enb,Bool),chan);
    genuhdset(:set_tx_subdev_spec,(:subdev_spec,Uhd_subdev_spec),mb);
    genuhdset(:set_tx_rate,(:rate,Cdouble),chan);
    genuhdset(:set_tx_lo_source,(:src,String),name,chan);
    genuhdset(:set_tx_lo_export_enabled,(:enabled,Bool),name,chan);
    genuhdset(:set_tx_lo_freq,name,chan);
    genuhdset(:set_tx_gain,(:gain,Cdouble),chan,(:gain_name,String));
    genuhdset(:set_normalized_tx_gain,(:gain,Cdouble),chan);
    genuhdset(:set_tx_antenna,(:ant,String),chan);
    genuhdset(:set_tx_bandwidth,(:bandwidth,Cdouble),chan);
    genuhdset(:set_gpio_attr,(:bank,String),(:attr,String),(:value,UInt32),(:mask,UInt32),mb);
end

## Uhd_usrp_clock
genuhdobj("uhd_usrp_clock",(:args,String));
genuhdget(:get_num_boards,Csize_t,prefix=:uhd_usrp_clock,t=Uhd_usrp_clock);
genuhdget(:get_time,UInt32,(:board,Csize_t),prefix=:uhd_usrp_clock,t=Uhd_usrp_clock);
genuhdget(:get_sensor,Uhd_sensor_value,(:name,String),(:board,Csize_t),prefix=:uhd_usrp_clock,t=Uhd_usrp_clock);
genuhdget(:get_sensor_names,Vector{String},(:board,Csize_t),prefix=:uhd_usrp_clock,t=Uhd_usrp_clock);

## set_thread_priority
export set_thread_priority;
set_thread_priority(priority::Cfloat,realtime::Bool)=uhd_set_thread_priority(priority,realtime);

end
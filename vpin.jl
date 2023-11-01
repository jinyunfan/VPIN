using DataFrames
using CSV
using ProgressMeter
using  Dates
using Statistics
using Distributions


function calcu_vpin(timebarsize, buckets, samplength, dataset)
  dataset=dataset[:,[1,2,3]]
  dataset[!, :price] = convert.(Float64, dataset[!, :price])
  dataset[!, :volume] = convert.(Float64, dataset[!, :volume])

  if eltype(dataset.timestamp) != DateTime
   dataset.timestamp = first.(dataset.timestamp,19)
   # 使用 transform! 对数据框的列进行操作
   transform!(dataset, :timestamp => ByRow(x -> DateTime(x, dateformat"yyyy-mm-dd HH:MM:SS"))=> :timestamp) 
  end
  dataset
  # initialize the local variables
  tbv =  vbs = bucket = nothing
  #estimatevpin = new("estimate.vpin")
  
  #先将数据按一定时间间隔分组
  time_interval = Dates.Second(timebarsize)  
  start_time = minimum(dataset.timestamp)
  end_time = maximum(dataset.timestamp)+time_interval
  time_series = DateTime[]
  current_time = start_time
  while current_time <= end_time
    push!(time_series, current_time)
    current_time += time_interval
  end
  start_time = end_time = current_time = nothing 

  dataset.bins = fill(0, length(dataset.timestamp))
  breaks = time_series[2:end]
  dataset.bins = map(x -> findfirst(y -> x <= y, breaks), dataset.timestamp)
 
  dataset.interval = time_series[dataset.bins]
  dataset = dataset[!,Not(:bins)]
  #dataset
  breaks = time_series = nothing
  #各组某列进行操作
  function diff(x)
     dp = last(x) - first(x)
     return dp
  end
  #minutebars诞生
  minutebars = combine(groupby(dataset,:interval),:price => diff =>:dp ,:volume => sum =>:tbv)
  dropmissing!(minutebars) #删除包含缺失值的行
  minutebars.id = 1:nrow(minutebars)
  
  sdp = std(minutebars.dp)
  
  ndays = length(unique(Dates.value.(Date.(minutebars.interval))))
  totvol = sum(minutebars.tbv)
  vbs = (totvol / ndays) / buckets
  
  #params = (tbSize = timebarsize, buckets = buckets, samplength = samplength, VBS = vbs, ndays = ndays)
  #estimatevpin@parameters = params
  
  #处理那些timebar中volume大于volume bucket size，即tbv>vbs的情况
  x = 10
  threshold = (1 - 1 / x) * vbs
  #largebars诞生
  largebars = filter( row -> row.tbv > threshold, minutebars)
  
  if nrow(largebars) != 0
   largebnum = findall(row -> row.tbv > threshold, eachrow(minutebars))
    #将那些tbv>vbs的行分为1行余数＋x*整数行threshold/x
    #其中整数和余数分别为tbv/threshold的整数和余数
   minutebars[largebnum, :tbv] .= minutebars[largebnum, :tbv] .% threshold
    
    #将largebars的每一行重复n_rep行
   new_largebars = DataFrame()
   for row in eachrow(largebars)
      n_rep= x * div(row.tbv, threshold)
      row.tbv = threshold / x
      for _ in 1:n_rep
        push!(new_largebars, row)
      end
   end
   n_rep = nothing

   minutebars = vcat(minutebars, new_largebars)
   sort!(minutebars, :interval )
   minutebars.id = 1:size(minutebars, 1)

   new_largebars = largebars = largebnum = nothing

  end
  
  #划分volume bucket
  minutebars.runvol = cumsum(minutebars.tbv)
  minutebars.bucket = 1 .+ div.(minutebars.runvol, vbs)
  
  minutebars.exvol = minutebars.runvol - (minutebars.bucket .- 1) * vbs
                                                                      
  xtrarows = combine(groupby(filter( row -> row.bucket != 1, minutebars), :bucket), first)
  xtrarnum  = findall(row -> row.id in xtrarows.id, eachrow(minutebars))
  
  minutebars[xtrarnum, :tbv] .= minutebars[xtrarnum, :exvol]  #这一个timebar中补完上一个buckets剩余的加入这一个buckets的volume量
  xtrarows.tbv = xtrarows.tbv - xtrarows.exvol  #这一个timebar往上一个buckets中补的volume量
  xtrarows.bucket = xtrarows.bucket .- 1
  
  xtrarows = xtrarows[!, ["interval", "dp", "tbv","id", "runvol", "bucket", "exvol"]]
  minutebars = vcat(minutebars, xtrarows)
  sort!(minutebars,[:interval, :bucket])
  xtrarows = xtrarnum = nothing

  #数据处理完毕 开始计算权重
  μ = 0.0
  sigma = 1.0
  d = Normal(μ, sigma)
  minutebars.zb = cdf(d, minutebars.dp ./ sdp)
  minutebars.zs = 1 .- cdf(d, minutebars.dp ./ sdp)
  d = nothing
  # Calculate Buy Volume (bvol) and Sell volume (svol) by multiplying timebar's
  # volume (tbv) by the corresponding probabilities zb and zs.
  
  minutebars.bvol = minutebars.tbv .* minutebars.zb
  minutebars.svol = minutebars.tbv .* minutebars.zs
  
  minutebars = minutebars[minutebars.tbv .> 0, :]
  
  #将每个bucket聚合起来 计算VtaoS=agg.svol和VtaoB=agg.bvol
  bucketdata = combine(groupby(minutebars, :bucket), :bvol => sum => :agg_bvol, :svol => sum => :agg_svol)
  bucketdata.aoi = abs.(bucketdata.agg_bvol .- bucketdata.agg_svol)
  
  bucketdata.starttime = combine(groupby(minutebars, :bucket), first).interval
  bucketdata.endtime = combine(groupby(minutebars, :bucket), last).interval
  
  minutebars = nothing

  #计算vpin
  bucketdata.cumoi = cumsum(bucketdata.aoi)
  lag_values = [missing for _ in 1:(samplength -1)]
  bucketdata.lagcumoi  = vcat(lag_values, 0,bucketdata.cumoi[1:end-samplength])
  
  bucketdata.vpin = (bucketdata.cumoi - bucketdata.lagcumoi) /  (samplength * vbs)
  bucketdata.vpin[samplength] = bucketdata.cumoi[samplength] /  (samplength * vbs)
  #bucketdata.duration = as.numeric(  difftime(bucketdata$endtime, bucketdata$starttime, units = "secs"))
  
  vpin=bucketdata.vpin[samplength:nrow(bucketdata)]
  describe(vpin)
  
  
  # Calculate daily VPINs
  bucketdata.day = Date.(bucketdata.starttime)
  mean_function = x -> mean(skipmissing(x))
  dailyvpin = combine(groupby(bucketdata, :day),:vpin => mean_function => :dvpin )
  bucketdata = select(bucketdata, Not(:day, :lagcumoi, :cumoi))

 return (dailyvpin, bucketdata)
end


import { BarChart, Bar, XAxis, YAxis, CartesianGrid, Tooltip, ResponsiveContainer, Cell } from 'recharts';

const RunnerRankingsHistogram = ({ runnerData }) => {
  if (!runnerData || !runnerData.runnerRankings) return null;

  // Transform the data for visualization
  const chartData = runnerData.runnerRankings.map((ranking, index) => {
    // Invert the rank so that #1 (best) appears highest
    // Using a simple inversion: if max rank is ~600, then inverted = 600 - rank
    const maxRank = Math.max(...runnerData.runnerRankings.map(r => r.runnersRank));
    const invertedRank = maxRank - ranking.runnersRank + 1;
    
    return {
      name: `Race ${index + 1}`,
      originalRank: ranking.runnersRank,
      invertedRank: invertedRank,
      coef: ranking.runnersCoef,
      points: ranking.runnersPoints,
      raceIndex: index
    };
  });

  // Find best and worst ranks for color coding
  const bestRank = Math.min(...runnerData.runnerRankings.map(r => r.runnersRank));
  const worstRank = Math.max(...runnerData.runnerRankings.map(r => r.runnersRank));

  const getBarColor = (rank) => {
    if (rank === bestRank) return '#10B981'; // Green for best
    if (rank === worstRank) return '#EF4444'; // Red for worst
    return '#CA8A04'; // Yellow for others
  };

  return (
    <div className="w-full max-w-4xl mx-auto p-4">
      <h4 className="text-yellow-400 font-mono mb-2 text-xl">
        Rankings for {runnerData.runnerName} ({runnerData.runnerRegNo})
      </h4>
      
      <div className="bg-black p-4 rounded border border-yellow-700">
        <ResponsiveContainer width="100%" height={400}>
          <BarChart data={chartData} margin={{ top: 20, right: 30, left: 20, bottom: 60 }}>
            <CartesianGrid strokeDasharray="3 3" stroke="#525252" />
            <XAxis 
              dataKey="name" 
              tick={{ fontSize: 12, fill: '#FDE047' }}
              angle={-45}
              textAnchor="end"
              height={80}
            />
            <YAxis
              tick={{ fontSize: 12, fill: '#FDE047' }}
              label={{ 
                value: 'Performance (Higher = Better Rank)', 
                angle: -90, 
                position: 'insideLeft',
                style: { textAnchor: 'middle', fill: '#FDE047' }
              }}
            />
            <Tooltip 
              contentStyle={{ 
                backgroundColor: '#111827', 
                border: '1px solid #CA8A04',
                color: '#FDE047',
                fontFamily: 'monospace'
              }}
              content={({ active, payload, label }) => {
                if (active && payload && payload.length) {
                  const data = payload[0].payload;
                  return (
                    <div style={{
                      backgroundColor: '#111827',
                      border: '1px solid #CA8A04',
                      color: '#FDE047',
                      fontFamily: 'monospace',
                      padding: '12px',
                      borderRadius: '4px',
                      minWidth: '200px'
                    }}>
                      <p style={{ fontWeight: 'bold', marginBottom: '8px' }}>{label}</p>
                      <p>Rank: #{data.originalRank}</p>
                      <p>Coefficient: {data.coef}</p>
                      <p>Points: {data.points.toLocaleString()}</p>
                      <p style={{ marginTop: '4px', fontSize: '11px', opacity: 0.8 }}>
                        {data.originalRank === bestRank ? '🏆 Best performance' : 
                         data.originalRank === worstRank ? '📉 Worst performance' : ''}
                      </p>
                    </div>
                  );
                }
                return null;
              }}
            />
            <Bar
              dataKey="invertedRank"
              style={{ cursor: 'pointer' }}
            >
              {chartData.map((entry, index) => (
                <Cell
                  key={`cell-${index}`}
                  fill={getBarColor(entry.originalRank)}
                />
              ))}
            </Bar>
          </BarChart>
        </ResponsiveContainer>
        
        <div className="mt-4 text-sm text-yellow-400 font-mono">
          <div className="flex flex-wrap gap-4 justify-center">
            <div className="flex items-center gap-2">
              <div className="w-4 h-4 bg-green-500 rounded"></div>
              <span>Best Rank (#{bestRank})</span>
            </div>
            <div className="flex items-center gap-2">
              <div className="w-4 h-4 bg-red-500 rounded"></div>
              <span>Worst Rank (#{worstRank})</span>
            </div>
            <div className="flex items-center gap-2">
              <div className="w-4 h-4 bg-yellow-600 rounded"></div>
              <span>Other Ranks</span>
            </div>
          </div>
          <p className="text-center mt-2 text-xs opacity-75">
            Higher bars = Better rankings (closer to #1)
          </p>
        </div>
      </div>
    </div>
  );
};

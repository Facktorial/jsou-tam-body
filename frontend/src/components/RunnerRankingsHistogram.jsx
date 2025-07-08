import { BarChart, Bar, XAxis, YAxis, CartesianGrid, Tooltip, ResponsiveContainer, Cell } from 'recharts';

const RunnerRankingsHistogram = ({
    runnerData,
    rankingsData,
    rankingType,
    setRankingType
}) => {
  if (!runnerData || !runnerData.runnerRankings) { return null; }
  //if (!rankingsData) { return null; }
  console.log(rankingsData);

  const barHeightOffset = 10;

  const simplifyRankingLabel = (label) => {
    const monthsMatch = label.match(/(\d+)\s*(měsíc(ů)?|events?)/i);
    const racesMatch = label.match(/(\d+)\s*(závod(ů)?|events?)/i);
    const prefix = label.split('-')[0].replace(/ranking/i, '').trim();

    const months = monthsMatch ? monthsMatch[1] + 'm' : '';
    const races = racesMatch ? racesMatch[1] : '';
    const suffix = `${months}${races}`;

    return suffix ? `${prefix} - ${suffix}` : prefix;
  }

  const chartData = runnerData.runnerRankings.map((ranking, index) => {
    // Invert the rank so that #1 (best) appears highest
    const maxRank = Math.max(...runnerData.runnerRankings.map(r => r.runnersRank));
    const invertedRank = maxRank - ranking.runnersRank + barHeightOffset;
    
    return {
      name: rankingsData[index].NameCZ,
      originalRank: ranking.runnersRank,
      invertedRank: invertedRank,
      coef: ranking.runnersCoef,
      points: ranking.runnersPoints,
      raceIndex: index
    };
  });

  const handleBarClick = (data) => {
    if (data && setRankingType) {
      setRankingType(data.raceIndex);
    }
  };

  const bestRank = Math.min(...runnerData.runnerRankings.map(r => r.runnersRank));
  const worstRank = Math.max(...runnerData.runnerRankings.map(r => r.runnersRank));

  const getBarColor = (rank) => {
    if (rank === bestRank) { return '#10B981' };
    if (rank === worstRank) { return '#EF4444' };
    return '#CA8A04';
  };

  const displayMinRank = worstRank + 10;
  const displayMaxRank = bestRank - 10;
  const displayRankDiff = displayMinRank - displayMaxRank;

  return (
    <div>
      <h4 className="text-yellow-400 font-mono mb-2">
        Rankings for {runnerData.runnerName}:
      </h4>
      
      <div className="bg-black p-4 rounded border border-yellow-700">
        <ResponsiveContainer width="100%" height={300}>
          <BarChart data={chartData} margin={{ top: 20, right: 30, left: 20, bottom: 30 }}>
            <CartesianGrid strokeDasharray="3 3" stroke="#525252" />
            <XAxis 
              dataKey="name" 
              tick={{ fontSize: 12, fill: '#FDE047' }}
              angle={-30}
              textAnchor="end"
              height={50}
              interval={0}
              tickFormatter={simplifyRankingLabel}
            />
            <YAxis
              tick={{ fontSize: 12, fill: '#FDE047' }}
              domain={[0, displayRankDiff]}
              label={{ 
                value: 'Standings by Ranking', 
                angle: -90, 
                position: 'insideLeft',
                style: { textAnchor: 'middle', fill: '#FDE047' }
              }}
              tickFormatter={(value) => `#${Math.round(displayMinRank - value)}`}
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
              onClick={handleBarClick}
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
        
        <div className="text-xs text-yellow-600 mt-2">
          <details>
            <summary className="cursor-pointer">Typy rankingů</summary>
            <ul className="mt-1 ml-4">
              { rankingsData.map((rankingType, index) => (
                  <li key={index} className="text-yellow-500">• {rankingType.NameCZ}</li>
              ))}
            </ul>
          </details>
        </div>

        <div className="mt-4 text-sm text-yellow-400 font-mono">
          <div className="text-center mt-2 text-xs opacity-75">
            <div className="flex items-center gap-2">
              <div className="w-4 h-4 bg-green-500 rounded"></div>
              <span>Best Rank (#{bestRank})</span>
            </div>
            <div className="flex items-center gap-2">
              <div className="w-4 h-4 bg-red-500 rounded"></div>
              <span>Worst Rank (#{worstRank})</span>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};

export default RunnerRankingsHistogram;

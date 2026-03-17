import { useState, useMemo } from "react";
import {
  RadarChart, Radar, PolarGrid, PolarAngleAxis, PolarRadiusAxis,
  LineChart, Line, XAxis, YAxis, CartesianGrid, Tooltip, Legend,
  ResponsiveContainer, AreaChart, Area, BarChart, Bar,
} from "recharts";

// --- Sample data generation ---
const DIMENSIONS = ["energy", "valence", "anxiety", "focus", "sociability"];
const DIM_COLORS = {
  energy: "#f59e0b",
  valence: "#10b981",
  anxiety: "#ef4444",
  focus: "#6366f1",
  sociability: "#ec4899",
};

function seededRandom(seed) {
  let s = seed;
  return () => {
    s = (s * 16807 + 0) % 2147483647;
    return (s - 1) / 2147483646;
  };
}

function generateMoodData(days = 90) {
  const rand = seededRandom(42);
  const data = [];
  const base = { energy: 0.55, valence: 0.6, anxiety: 0.35, focus: 0.5, sociability: 0.45 };
  const drift = { energy: 0, valence: 0, anxiety: 0, focus: 0, sociability: 0 };

  for (let i = 0; i < days; i++) {
    const date = new Date(2026, 0, 1);
    date.setDate(date.getDate() + i);
    const dayOfWeek = date.getDay();
    const entry = { date: date.toISOString().slice(0, 10), dayOfWeek };

    for (const dim of DIMENSIONS) {
      drift[dim] += (rand() - 0.5) * 0.08;
      drift[dim] *= 0.92;
      let weekendEffect = 0;
      if (dayOfWeek === 0 || dayOfWeek === 6) {
        weekendEffect = dim === "energy" ? 0.08 : dim === "anxiety" ? -0.1 : dim === "sociability" ? 0.12 : 0;
      }
      const val = base[dim] + drift[dim] + weekendEffect + (rand() - 0.5) * 0.12;
      entry[dim] = Math.max(0, Math.min(1, val));
    }
    data.push(entry);
  }
  return data;
}

const allData = generateMoodData(90);

// --- Helpers ---
const avg = (arr, key) => arr.reduce((s, d) => s + d[key], 0) / arr.length;

function getWeekData(data) {
  const weeks = [];
  for (let i = 0; i < data.length; i += 7) {
    const chunk = data.slice(i, i + 7);
    const entry = { week: `W${Math.floor(i / 7) + 1}`, weekStart: chunk[0].date };
    for (const dim of DIMENSIONS) entry[dim] = Math.round(avg(chunk, dim) * 100) / 100;
    weeks.push(entry);
  }
  return weeks;
}

// --- Components ---

function RadarView({ data, range }) {
  const slice = data.slice(-range);
  const averaged = DIMENSIONS.map((dim) => ({
    dimension: dim.charAt(0).toUpperCase() + dim.slice(1),
    value: Math.round(avg(slice, dim) * 100),
    fullMark: 100,
  }));

  const recent7 = data.slice(-7);
  const prior7 = data.slice(-14, -7);
  const comparison = DIMENSIONS.map((dim) => ({
    dimension: dim.charAt(0).toUpperCase() + dim.slice(1),
    thisWeek: Math.round(avg(recent7, dim) * 100),
    lastWeek: Math.round(avg(prior7, dim) * 100),
    fullMark: 100,
  }));

  return (
    <div>
      <h3 className="text-lg font-semibold text-gray-200 mb-1">Radar / Spider Chart</h3>
      <p className="text-sm text-gray-400 mb-4">
        Shows all 5 dimensions at once as a shape. Great for comparing your overall mood
        "fingerprint" between time periods. Overlapping radars reveal shifts at a glance.
      </p>
      <div className="flex flex-wrap gap-4">
        <div className="flex-1 min-w-[280px]">
          <p className="text-xs text-gray-500 mb-1 text-center">Average — last {range} days</p>
          <ResponsiveContainer width="100%" height={260}>
            <RadarChart data={averaged}>
              <PolarGrid stroke="#334155" />
              <PolarAngleAxis dataKey="dimension" tick={{ fill: "#94a3b8", fontSize: 11 }} />
              <PolarRadiusAxis domain={[0, 100]} tick={false} axisLine={false} />
              <Radar dataKey="value" stroke="#6366f1" fill="#6366f1" fillOpacity={0.25} strokeWidth={2} />
            </RadarChart>
          </ResponsiveContainer>
        </div>
        <div className="flex-1 min-w-[280px]">
          <p className="text-xs text-gray-500 mb-1 text-center">This week vs last week</p>
          <ResponsiveContainer width="100%" height={260}>
            <RadarChart data={comparison}>
              <PolarGrid stroke="#334155" />
              <PolarAngleAxis dataKey="dimension" tick={{ fill: "#94a3b8", fontSize: 11 }} />
              <PolarRadiusAxis domain={[0, 100]} tick={false} axisLine={false} />
              <Radar dataKey="thisWeek" stroke="#10b981" fill="#10b981" fillOpacity={0.2} strokeWidth={2} name="This week" />
              <Radar dataKey="lastWeek" stroke="#64748b" fill="#64748b" fillOpacity={0.1} strokeWidth={1.5} strokeDasharray="4 3" name="Last week" />
              <Legend wrapperStyle={{ fontSize: 11, color: "#94a3b8" }} />
            </RadarChart>
          </ResponsiveContainer>
        </div>
      </div>
    </div>
  );
}

function TimelineView({ data }) {
  const formatted = data.map((d) => ({
    ...d,
    label: d.date.slice(5),
    energy: Math.round(d.energy * 100),
    valence: Math.round(d.valence * 100),
    anxiety: Math.round(d.anxiety * 100),
    focus: Math.round(d.focus * 100),
    sociability: Math.round(d.sociability * 100),
  }));

  return (
    <div>
      <h3 className="text-lg font-semibold text-gray-200 mb-1">Timeline / Multi-line</h3>
      <p className="text-sm text-gray-400 mb-4">
        Each dimension as a line over time. Best for spotting trends, correlations between
        dimensions (e.g. anxiety spikes when focus drops), and seasonal patterns.
      </p>
      <ResponsiveContainer width="100%" height={280}>
        <LineChart data={formatted} margin={{ top: 5, right: 10, bottom: 5, left: -10 }}>
          <CartesianGrid strokeDasharray="3 3" stroke="#1e293b" />
          <XAxis dataKey="label" tick={{ fill: "#64748b", fontSize: 10 }} interval={6} />
          <YAxis domain={[0, 100]} tick={{ fill: "#64748b", fontSize: 10 }} />
          <Tooltip
            contentStyle={{ background: "#1e293b", border: "1px solid #334155", borderRadius: 8, fontSize: 12 }}
            labelStyle={{ color: "#94a3b8" }}
          />
          {DIMENSIONS.map((dim) => (
            <Line key={dim} type="monotone" dataKey={dim} stroke={DIM_COLORS[dim]} strokeWidth={1.5} dot={false} />
          ))}
          <Legend wrapperStyle={{ fontSize: 11 }} />
        </LineChart>
      </ResponsiveContainer>
    </div>
  );
}

function StackedAreaView({ data }) {
  const weekly = getWeekData(data).map((w) => ({
    ...w,
    energy: Math.round(w.energy * 100),
    valence: Math.round(w.valence * 100),
    anxiety: Math.round(w.anxiety * 100),
    focus: Math.round(w.focus * 100),
    sociability: Math.round(w.sociability * 100),
  }));

  return (
    <div>
      <h3 className="text-lg font-semibold text-gray-200 mb-1">Stacked Area (weekly)</h3>
      <p className="text-sm text-gray-400 mb-4">
        Shows how each dimension contributes to your overall mood "budget" over time.
        Good for understanding the composition of your emotional state week-to-week.
      </p>
      <ResponsiveContainer width="100%" height={280}>
        <AreaChart data={weekly} margin={{ top: 5, right: 10, bottom: 5, left: -10 }}>
          <CartesianGrid strokeDasharray="3 3" stroke="#1e293b" />
          <XAxis dataKey="week" tick={{ fill: "#64748b", fontSize: 10 }} />
          <YAxis tick={{ fill: "#64748b", fontSize: 10 }} />
          <Tooltip contentStyle={{ background: "#1e293b", border: "1px solid #334155", borderRadius: 8, fontSize: 12 }} />
          {DIMENSIONS.map((dim) => (
            <Area key={dim} type="monotone" dataKey={dim} stackId="1" stroke={DIM_COLORS[dim]} fill={DIM_COLORS[dim]} fillOpacity={0.35} />
          ))}
          <Legend wrapperStyle={{ fontSize: 11 }} />
        </AreaChart>
      </ResponsiveContainer>
    </div>
  );
}

function HeatmapView({ data }) {
  const weeks = [];
  const startDate = new Date(data[0].date);
  const startDay = startDate.getDay();

  const lookup = {};
  data.forEach((d) => { lookup[d.date] = d; });

  for (let w = 0; w < 13; w++) {
    for (let d = 0; d < 7; d++) {
      const offset = w * 7 + d - startDay;
      const dt = new Date(startDate);
      dt.setDate(dt.getDate() + offset);
      const key = dt.toISOString().slice(0, 10);
      weeks.push({ week: w, day: d, date: key, entry: lookup[key] || null });
    }
  }

  const dayLabels = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];
  const [heatDim, setHeatDim] = useState("valence");

  return (
    <div>
      <h3 className="text-lg font-semibold text-gray-200 mb-1">Calendar Heatmap</h3>
      <p className="text-sm text-gray-400 mb-3">
        GitHub-contribution-style grid for one dimension at a time. Instantly shows
        patterns by day-of-week and helps spot streaks or dips.
      </p>
      <div className="flex gap-2 mb-3 flex-wrap">
        {DIMENSIONS.map((dim) => (
          <button
            key={dim}
            onClick={() => setHeatDim(dim)}
            className={`px-3 py-1 rounded-full text-xs font-medium transition-colors ${
              heatDim === dim ? "text-white" : "text-gray-400 hover:text-gray-200"
            }`}
            style={{ backgroundColor: heatDim === dim ? DIM_COLORS[dim] + "cc" : "#1e293b", border: `1px solid ${heatDim === dim ? DIM_COLORS[dim] : "#334155"}` }}
          >
            {dim}
          </button>
        ))}
      </div>
      <div className="flex gap-1">
        <div className="flex flex-col gap-1 mr-1 mt-0">
          {dayLabels.map((l, i) => (
            <div key={i} className="h-5 flex items-center text-xs text-gray-500 leading-none" style={{ fontSize: 9 }}>
              {i % 2 === 1 ? l : ""}
            </div>
          ))}
        </div>
        <div className="flex gap-1">
          {Array.from({ length: 13 }, (_, w) => (
            <div key={w} className="flex flex-col gap-1">
              {Array.from({ length: 7 }, (_, d) => {
                const cell = weeks.find((c) => c.week === w && c.day === d);
                const val = cell?.entry?.[heatDim];
                const opacity = val != null ? 0.15 + val * 0.85 : 0.04;
                const color = DIM_COLORS[heatDim];
                return (
                  <div
                    key={d}
                    className="w-5 h-5 rounded-sm"
                    style={{ backgroundColor: color, opacity }}
                    title={cell?.date ? `${cell.date}: ${val != null ? (val * 100).toFixed(0) + "%" : "no data"}` : ""}
                  />
                );
              })}
            </div>
          ))}
        </div>
      </div>
      <div className="flex items-center gap-1 mt-2 ml-6">
        <span className="text-xs text-gray-500">Low</span>
        {[0.1, 0.3, 0.5, 0.7, 0.9].map((v) => (
          <div key={v} className="w-4 h-4 rounded-sm" style={{ backgroundColor: DIM_COLORS[heatDim], opacity: 0.15 + v * 0.85 }} />
        ))}
        <span className="text-xs text-gray-500">High</span>
      </div>
    </div>
  );
}

function DimensionBarView({ data, range }) {
  const slice = data.slice(-range);
  const barData = DIMENSIONS.map((dim) => ({
    dimension: dim.charAt(0).toUpperCase() + dim.slice(1),
    value: Math.round(avg(slice, dim) * 100),
    fill: DIM_COLORS[dim],
  }));

  return (
    <div>
      <h3 className="text-lg font-semibold text-gray-200 mb-1">Dimension Summary Bar</h3>
      <p className="text-sm text-gray-400 mb-4">
        Simple horizontal bars showing your average for each dimension. Good as a dashboard
        "at-a-glance" widget alongside more detailed views.
      </p>
      <div className="space-y-2">
        {barData.map((d) => (
          <div key={d.dimension} className="flex items-center gap-3">
            <span className="text-xs text-gray-400 w-20 text-right">{d.dimension}</span>
            <div className="flex-1 h-6 bg-gray-800 rounded-full overflow-hidden relative">
              <div
                className="h-full rounded-full transition-all duration-500"
                style={{ width: `${d.value}%`, backgroundColor: d.fill, opacity: 0.75 }}
              />
              <span className="absolute right-2 top-0 h-full flex items-center text-xs text-gray-300 font-medium">
                {d.value}%
              </span>
            </div>
          </div>
        ))}
      </div>
    </div>
  );
}

// --- Main ---
export default function MoodVizSampler() {
  const [activeTab, setActiveTab] = useState("all");
  const [range, setRange] = useState(30);

  const tabs = [
    { id: "all", label: "All Views" },
    { id: "radar", label: "Radar" },
    { id: "timeline", label: "Timeline" },
    { id: "area", label: "Stacked Area" },
    { id: "heatmap", label: "Heatmap" },
    { id: "bars", label: "Summary Bars" },
  ];

  const views = {
    radar: <RadarView data={allData} range={range} />,
    timeline: <TimelineView data={allData.slice(-range)} />,
    area: <StackedAreaView data={allData.slice(-range)} />,
    heatmap: <HeatmapView data={allData} />,
    bars: <DimensionBarView data={allData} range={range} />,
  };

  return (
    <div className="min-h-screen bg-gray-950 text-gray-100 p-6 font-sans">
      <div className="max-w-4xl mx-auto">
        <h1 className="text-2xl font-bold mb-1">ish — Mood Visualization Sampler</h1>
        <p className="text-sm text-gray-400 mb-6">
          90 days of generated mood data across 5 dimensions. Explore each chart type
          to see what fits your interactive site best.
        </p>

        <div className="flex flex-wrap items-center gap-3 mb-6">
          <div className="flex gap-1 bg-gray-900 rounded-lg p-1">
            {tabs.map((tab) => (
              <button
                key={tab.id}
                onClick={() => setActiveTab(tab.id)}
                className={`px-3 py-1.5 rounded-md text-xs font-medium transition-colors ${
                  activeTab === tab.id ? "bg-indigo-600 text-white" : "text-gray-400 hover:text-gray-200 hover:bg-gray-800"
                }`}
              >
                {tab.label}
              </button>
            ))}
          </div>
          {activeTab !== "heatmap" && (
            <select
              value={range}
              onChange={(e) => setRange(Number(e.target.value))}
              className="bg-gray-900 border border-gray-700 rounded-md px-2 py-1.5 text-xs text-gray-300"
            >
              <option value={7}>Last 7 days</option>
              <option value={14}>Last 14 days</option>
              <option value={30}>Last 30 days</option>
              <option value={60}>Last 60 days</option>
              <option value={90}>All 90 days</option>
            </select>
          )}
        </div>

        <div className="space-y-8">
          {activeTab === "all"
            ? Object.entries(views).map(([key, view]) => (
                <div key={key} className="bg-gray-900 border border-gray-800 rounded-xl p-5">
                  {view}
                </div>
              ))
            : (
                <div className="bg-gray-900 border border-gray-800 rounded-xl p-5">
                  {views[activeTab]}
                </div>
              )}
        </div>

        <div className="mt-8 bg-gray-900 border border-gray-800 rounded-xl p-5">
          <h3 className="text-lg font-semibold text-gray-200 mb-3">Quick Comparison</h3>
          <div className="overflow-x-auto">
            <table className="w-full text-xs text-gray-300">
              <thead>
                <tr className="border-b border-gray-800">
                  <th className="text-left py-2 px-2 text-gray-400 font-medium">Chart</th>
                  <th className="text-left py-2 px-2 text-gray-400 font-medium">Best for</th>
                  <th className="text-left py-2 px-2 text-gray-400 font-medium">Interactivity</th>
                  <th className="text-left py-2 px-2 text-gray-400 font-medium">Data density</th>
                </tr>
              </thead>
              <tbody>
                {[
                  ["Radar", "Mood shape comparison", "Hover, toggle periods", "Low-Med"],
                  ["Timeline", "Trends & correlations", "Brush/zoom, toggle dims", "High"],
                  ["Stacked Area", "Composition over time", "Hover breakdowns", "Medium"],
                  ["Heatmap", "Day-of-week patterns, streaks", "Click to drill into day", "High"],
                  ["Summary Bars", "Dashboard at-a-glance", "Tap to expand", "Low"],
                ].map(([chart, best, interact, density]) => (
                  <tr key={chart} className="border-b border-gray-800/50">
                    <td className="py-2 px-2 font-medium text-gray-200">{chart}</td>
                    <td className="py-2 px-2">{best}</td>
                    <td className="py-2 px-2">{interact}</td>
                    <td className="py-2 px-2">{density}</td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        </div>
      </div>
    </div>
  );
}
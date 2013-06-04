#pragma once

#include <program_opts/app_options.h>
#include <gringo/output.h>
#include <gringo/inclit.h>
#include <gringo/converter.h>
#include <gringo/streams.h>
#include "clingo/clasp/clasp_options.h"
#include "clingo/clasp/clasp_output.h"
#include <clasp/smodels_constraints.h>
#include "gringo/gringo_app.h"
#include "clingo/clingo_options.h"
#include "clingo/claspoutput.h"
#include "clingo/timer.h"
#include <iomanip>
#include <iostream>
#include <utility>
#include <stdlib.h>
#include <stdio.h>
#include <boost/regex.hpp>

using namespace Clasp;
class GCAspApp : public GringoApp, public Clasp::ClaspFacade::Callback
{
private:
	class LuaImpl;
	typedef std::auto_ptr<LuaImpl> LuaImplPtr;
	LuaImplPtr luaImpl;

public:
	/** returns a singleton instance */
	static GCAspApp& instance();

protected:
	// AppOptions interface
	void initOptions(ProgramOptions::OptionGroup& root, ProgramOptions::OptionGroup& hidden)
	{
		config_.solver = &solver_;
		cmdOpts_.setConfig(&config_);
		cmdOpts_.initOptions(root, hidden);
		clingo.initOptions(root, hidden);
		generic.verbose = 1;
		GringoApp::initOptions(root, hidden);
	}

	void addDefaults(std::string& defaults)
	{
		cmdOpts_.addDefaults(defaults);
		clingo.addDefaults(defaults);
		defaults += "  --verbose=1";
		GringoApp::addDefaults(defaults);
	}

	bool validateOptions(ProgramOptions::OptionValues& v, Messages& m)
	{
		if (cmdOpts_.basic.timeout != -1)
			m.warning.push_back("Time limit not supported");
		return cmdOpts_.validateOptions(v, m)
			&& GringoApp::validateOptions(v, m)
			&& clingo.validateOptions(v, GringoApp::gringo, m);
	}
	// ---------------------------------------------------------------------------------------

	// Application interface
	void printVersion() const;
	std::string getVersion() const;
	std::string getUsage()   const { return "[number] [options] [files]"; }
	ProgramOptions::PosOption getPositionalParser() const { return &Clasp::parsePositional; }
	void handleSignal(int sig);
	int  doRun();
	// -------------------------------------------------------------------------------------------

	// ClaspFacade::Callback interface
	void state(Clasp::ClaspFacade::Event e, Clasp::ClaspFacade& f);
	void event(Clasp::ClaspFacade::Event e, Clasp::ClaspFacade& f);
	void warning(const char* msg) { messages.warning.push_back(msg); }
	// -------------------------------------------------------------------------------------------

	enum ReasonEnd { reason_timeout, reason_interrupt, reason_end };
	enum { numStates = Clasp::ClaspFacade::num_states };
	void printResult(ReasonEnd re);
	void configureInOut(Streams& s);

	typedef std::auto_ptr<Clasp::OutputFormat> ClaspOutPtr;
	typedef std::auto_ptr<Clasp::Input> ClaspInPtr;

	Clasp::Solver          solver_;           // solver to use for search
	Clasp::SolveStats      stats_;            // accumulates clasp solve stats in incremental setting
	Clasp::ClaspConfig     config_;           // clasp configuration - from command-line
	Clasp::ClaspOptions    cmdOpts_;          // clasp basic options - from command-line
	Timer                  timer_[numStates]; // one timer for each state
	ClaspOutPtr            out_;              // printer for printing result of search
	ClaspInPtr             in_;               // input for clasp
	Clasp::ClaspFacade*    facade_;           // interface to clasp lib
public:
	ClingoOptions<ICLINGO> clingo;                  // (i)clingo options   - from command-line
};

struct GCInput: public Clasp::Input
{
	typedef std::auto_ptr<ClaspOutput> OutputPtr;
	typedef Clasp::MinimizeConstraint* MinConPtr;

	GCInput(std::istream &s);
	Format format() const { return Clasp::Input::SMODELS; }
	MinConPtr getMinimize(Clasp::Solver& s, Clasp::ProgramBuilder* api, bool heu) { return api ? api->createMinimize(s, heu) : 0; }
	void getAssumptions(Clasp::LitVec& a);
	bool read(Clasp::Solver& s, Clasp::ProgramBuilder* api, int);
	void addColor(Clasp::ProgramBuilder *api);
    /*
	OutputPtr              out;
	IncConfig              config;
	Clasp::Solver*         solver;
    */
    unsigned int nNodes;
    typedef unsigned int uint;
    typedef std::pair<uint, uint> Edge;
    typedef std::vector<Edge> EdgeVec;
    EdgeVec edges;
    Clasp::LitVec assumptions;
    uint k; // current level.
};

#ifdef WITH_LUA
#	include "clingo/lua_impl.h"
#else

class GCAspApp::LuaImpl
{
public:
	LuaImpl(Grounder *, Clasp::Solver *, ClaspOutput *) { }
	bool locked() { return false; }
	void onModel() { }
	void onBeginStep() { }
	void onEndStep() { }
};

#endif

/////////////////////////////////////////////////////////////////////////////////////////
// GCInput
/////////////////////////////////////////////////////////////////////////////////////////
#define LINESIZE 256
inline int match2int(boost::csub_match m) {
    return atoi(m.str().c_str());
}

GCInput::GCInput(std::istream &ifs): nNodes(0), k(0)
{

    boost::regex node("(\\w+)\\((\\d+)\\)\\.");
    boost::regex edge("(\\w+)\\((\\d+)\\,\\s*(\\d+)\\)\\.");
    //typedef std::match_results<const char*> cmatch;
    char line[LINESIZE];
    boost::cmatch mn, me;
    while (ifs.good()) {
        ifs.getline(line, LINESIZE - 1);
        if (!ifs.good()) break;
        if (boost::regex_match(line, mn, node) && mn.size() > 1) {
            uint n = match2int(mn[2]);
            if (nNodes < n) {
                nNodes = n;
            }
        } else if (boost::regex_match(line, me, edge) && me.size() > 2) {
            uint s = match2int(me[2]);
            uint t = match2int(me[3]);
            edges.push_back(std::make_pair(s, t));
        }
    }
    std::cout << "Num of Nodes: " << nNodes << std::endl;
    std::cout << "Num of Edges: " << edges.size() << std::endl;
}

void GCInput::getAssumptions(Clasp::LitVec& a)
{
    //foreach(Literal &it, assumptions) { a.push_back(it); }
    a.reserve(assumptions.size());
    std::copy(assumptions.begin(), assumptions.end(), a.begin());
}

void GCInput::addColor(Clasp::ProgramBuilder *api) {
	 // start add rules.
	    uint offset = 3;//edges.size();
	    // edge facts.
	    /*for (int i = 0; i < offset; i++) {
	    	api->startRule();
	    	api->addHead(i + 1);
	    	api->endRule();
	    }*/
	    for (int i = 1; i <= nNodes; i++) {
	        // color(X, C) :- not ocolor(X, C).
	        api->startRule();
	        int h = 2 * nNodes * (k - 1) + 2 * (i - 1) + offset; // color(i, k)
	        api->addHead(h);
	        api->addToBody(h + 1, false);
	        api->endRule();

	        // ocolor(X, C) :- color(X, C1), C1 != C.
	        for (int j = 1; j < k; j++) {
	            api->startRule();
	            api->addHead(h + 1);
	            int b = 2 * nNodes * (j - 1) + 2 * (i - 1) + offset; // color(i, j)
	            api->addToBody(b, true);
	            api->endRule();

	            api->startRule();
	            api->addHead(b + 1);
	            api->addToBody(h, true);
	            api->endRule();
	        }
	    }
	    // constraints
	    // :- edge(X, Y), color(X, C), color(Y, C).
	    for (int i = 0; i < edges.size(); i++) {
	        api->startRule();
	        api->addHead(2);
	        //api->addToBody(i + 1, true);
	        int x = 2 * (edges[i].first - 1) + 2 * nNodes * (k - 1) + offset;
	        int y = 2 * (edges[i].second - 1) + 2 * nNodes * (k - 1) + offset;
	        api->addToBody(x, true);
	        api->addToBody(y, true);
	        api->endRule();
	    }
	    api->setCompute(2, false);

	    for (int i = 0; i < nNodes; i++) {
	        //v = api->newAtom();
	    	int idx = 2 * nNodes * (k - 1) + 2 * i + offset; // color(i + 1, k)
	        std::stringstream ss;
	        ss << "color(" << i + 1 << ", " << k << ")";
	        api->setAtomName(idx, ss.str().c_str());

	        //v = api->newAtom(true); // unsafe atom.
	        ss.str("");
	        ss.clear();
	        ss << "ocolor(" << i + 1 << ", " << k << ")";
	        api->setAtomName(idx + 1, ss.str().c_str());
	        api->setUnsafe(idx + 1);
	    }
}

bool GCInput::read(Clasp::Solver& s, Clasp::ProgramBuilder* api, int)
{
    k++;
    /*if (k == 1) {
        foreach(Edge &e, edges) {
            v = api->newAtom();
            std::stringstream ss;
            ss << "edge(" << e.first << ", " << e.second << ")";
            api->setAtomName(v, ss.str().c_str());
        }
    }*/
    if (k == 1) {
    	addColor(api);
        k++;
    	addColor(api);
    }
    else {
    	addColor(api);
    }
	return true;
}


/////////////////////////////////////////////////////////////////////////////////////////
// GCAspApp
/////////////////////////////////////////////////////////////////////////////////////////

GCAspApp &GCAspApp::instance()
{
	static GCAspApp app;
	return app;
}

void GCAspApp::printVersion() const
{
	using namespace std;
	GringoApp::printVersion();
	cout << endl;
	cout << "clasp " << CLASP_VERSION << "\n";
	cout << "Copyright (C) Benjamin Kaufmann" << "\n";
	cout << "License GPLv2+: GNU GPL version 2 or later <http://gnu.org/licenses/gpl.html>\n";
	cout << "clasp is free software: you are free to change and redistribute it.\n";
	cout << "There is NO WARRANTY, to the extent permitted by law." << endl;
}

std::string GCAspApp::getVersion() const {
	std::string r(GRINGO_VERSION);
	r += " (clasp ";
	r += CLASP_VERSION;
	r += ")";
	return r;
}

void GCAspApp::handleSignal(int) {
	for(int i = 0; i != sizeof(timer_)/sizeof(Timer); ++i)
		timer_[i].stop();
	fprintf(stderr, "\n*** INTERRUPTED! ***\n");
	if(facade_ && facade_->state() != Clasp::ClaspFacade::state_not_started)
		printResult(GCAspApp::reason_interrupt);
	_exit(solver_.stats.solve.models != 0 ?  S_SATISFIABLE : S_UNKNOWN);
}

void GCAspApp::configureInOut(Streams& s)
{
	using namespace Clasp;
	in_.reset(0);
	facade_ = 0;
	if(clingo.mode == CLASP)
	{
		s.open(generic.input);
		if (generic.input.size() > 1) { messages.warning.push_back("Only first file will be used"); }
		in_.reset(new StreamInput(s.currentStream(), detectFormat(s.currentStream())));
	}
	else
	{
		s.open(generic.input, constStream());
		in_.reset(new GCInput(std::cin)); //FIXME: hard coded into stdin stream.
	}
	if(config_.onlyPre)
	{
		if(clingo.mode == CLASP || clingo.mode == CLINGO) { generic.verbose = 0; }
		else { warning("Option '--pre' is ignored in incremental setting!"); config_.onlyPre = false; }
	}
	if(in_->format() == Input::SMODELS)
	{
		out_.reset(new AspOutput(cmdOpts_.basic.asp09));
		if(cmdOpts_.basic.asp09) { generic.verbose = 0; }
	}
	else if(in_->format() == Input::DIMACS) { out_.reset(new SatOutput()); }
	else if(in_->format() == Input::OPB)    { out_.reset(new PbOutput(generic.verbose > 1));  }
}

int GCAspApp::doRun()
{
	using namespace Clasp;
	if (gringo.groundOnly) { return GringoApp::doRun(); }
	if (cmdOpts_.basic.stats > 1) { 
		solver_.stats.solve.enableJumpStats(); 
		stats_.enableJumpStats();
	}
	Streams s;
	configureInOut(s);
	ClaspFacade clasp;
	facade_ = &clasp;
	timer_[0].start();
	if (clingo.mode == CLASP || clingo.mode == CLINGO)
	{
		clingo.iStats = false;
		clasp.solve(*in_, config_, this);
	}
	else { clasp.solveIncremental(*in_, config_, clingo.inc, this); }
	timer_[0].stop();
	printResult(reason_end);
	if      (clasp.result() == ClaspFacade::result_unsat) { return S_UNSATISFIABLE; }
	else if (clasp.result() == ClaspFacade::result_sat)   { return S_SATISFIABLE; }
	else                                                  { return S_UNKNOWN; }
}

#define STATUS(v1,x) if (generic.verbose<v1);else (x)

void GCAspApp::state(Clasp::ClaspFacade::Event e, Clasp::ClaspFacade& f) {
	using namespace Clasp;
	using namespace std;
	if (e == ClaspFacade::event_state_enter)
	{
		MainApp::printWarnings();
		if (f.state() == ClaspFacade::state_read)
		{
			if (f.step() == 0)
			{
				STATUS(2, cout << getExecutable() << " version " << getVersion() << "\n");
			}
			if (clingo.iStats)
			{
				cout << "=============== step " << f.step()+1 << " ===============" << endl;
			}
			STATUS(2, cout << "Reading      : ");
		}
		else if (f.state() == ClaspFacade::state_preprocess)
		{
			STATUS(2, cout << "Preprocessing: ");
		}
		else if (f.state() == ClaspFacade::state_solve)
		{
			STATUS(2, cout << "Solving...\n");
		}
		cout << flush;
		timer_[f.state()].start();

	}
	else if (e == ClaspFacade::event_state_exit)
	{
		timer_[f.state()].stop();
		if (generic.verbose > 1 && (f.state() == ClaspFacade::state_read || f.state() == ClaspFacade::state_preprocess))
			cout << fixed << setprecision(3) << timer_[f.state()].elapsed() << endl;
		if (f.state() == ClaspFacade::state_solve)
		{
			stats_.accu(solver_.stats.solve);
			if (clingo.iStats)
			{
				timer_[0].stop();
				cout << "\nModels   : " << solver_.stats.solve.models << "\n"
						 << "Time     : " << fixed << setprecision(3) << timer_[0].current() << " (g: " << timer_[ClaspFacade::state_read].current()
						 << ", p: " << timer_[ClaspFacade::state_preprocess].current() << ", s: " << timer_[ClaspFacade::state_solve].current() << ")\n"
						 << "Rules    : " << f.api()->stats.rules[0] << "\n"
						 << "Choices  : " << solver_.stats.solve.choices   << "\n"
						 << "Conflicts: " << solver_.stats.solve.conflicts << "\n";
				timer_[0].start();
			}
			solver_.stats.solve.reset();
		}
	}
}

void GCAspApp::event(Clasp::ClaspFacade::Event e, Clasp::ClaspFacade& f)
{
	using namespace std;
	using namespace Clasp;
	if (e == ClaspFacade::event_model)
	{
		if (!cmdOpts_.basic.quiet)
		{
			if ( !(config_.enumerate.consequences()) )
			{
				STATUS(1, cout << "Answer: " << solver_.stats.solve.models << endl);
				out_->printModel(solver_, *config_.solve.enumerator());
			}
			else
			{
				STATUS(1, cout << config_.enumerate.cbType() << " consequences:" << endl);
				out_->printConsequences(solver_, *config_.solve.enumerator());
			}
			if (config_.solve.enumerator()->minimize())
			{
				out_->printOptimize(*config_.solve.enumerator()->minimize());
			}
		}
	}
	else if (e == ClaspFacade::event_p_prepared)
	{
		if (config_.onlyPre)
		{
			if (f.api()) f.releaseApi(); // keep api so that we can later print the program
			else { STATUS(0, cout << "Vars: " << solver_.numVars() << " Constraints: " <<  solver_.numConstraints()<<endl); }
			AtomIndex* x = solver_.strategies().symTab.release();
			solver_.reset(); // release constraints and strategies - no longer needed
			solver_.strategies().symTab.reset(x);
		}
		else { out_->initSolve(solver_, f.api(), f.config()->solve.enumerator()); }
	}
}

void GCAspApp::printResult(ReasonEnd end)
{
	using namespace std;
	using namespace Clasp;
	if (config_.onlyPre)
	{
		if (end != reason_end) { return; }
		if (facade_->api())
		{
			facade_->result() == ClaspFacade::result_unsat
				? (void)(std::cout << "0\n0\nB+\n1\n0\nB-\n1\n0\n0\n")
				: facade_->api()->writeProgram(std::cout);
			delete facade_->releaseApi();
		}
		else
		{
			if (facade_->result() != ClaspFacade::result_unsat)
			{
				STATUS(0, cout << "Search not started because of option '--pre'!" << endl);
			}
			cout << "S UNKNWON" << endl;
		}
		return;
	}
	bool complete        = end == reason_end && !facade_->more();
	Solver& s            = solver_;
	s.stats.solve.accu(stats_);
	const Enumerator& en = *config_.solve.enumerator();
	if (clingo.iStats) { cout << "=============== Summary ===============" << endl; }
	out_->printSolution(s, en, complete);
	if (cmdOpts_.basic.quiet && config_.enumerate.consequences() && s.stats.solve.models != 0)
	{
		STATUS(1, cout << config_.enumerate.cbType() << " consequences:\n");
		out_->printConsequences(s, en);
	}
	if (generic.verbose > 0) 
	{
		const char* c= out_->format[OutputFormat::comment];
		const int   w= 12-(int)strlen(c);
		if      (end == reason_timeout)  { cout << "\n" << c << "TIME LIMIT  : 1\n"; }
		else if (end == reason_interrupt){ cout << "\n" << c << "INTERRUPTED : 1\n"; }
		uint64 enumerated = s.stats.solve.models;
		uint64 models     = enumerated;
		if      (config_.enumerate.consequences() && enumerated > 1) { models = 1; }
		else if (en.minimize())                                      { models = en.minimize()->models(); }
		cout << "\n" << c << left << setw(w) << "Models" << ": ";
		if (!complete)
		{
			char buf[64];
			int wr    = sprintf(buf, "%" PRIu64, models);
			buf[wr]   = '+';
			buf[wr+1] = 0;
			cout << setw(6) << buf << "\n";
		}
		else { cout << setw(6) << models << "\n"; }
		if (enumerated)
		{
			if (enumerated != models)
			{
				cout << c << setw(w) << "  Enumerated" << ": " << enumerated << "\n";
			}
			if (config_.enumerate.consequences())
			{
				cout << c <<"  " <<  setw(w-2) << config_.enumerate.cbType() << ": " << (complete?"yes":"unknown") << "\n";
			}
			if (en.minimize())
			{
				cout << c << setw(w) << "  Optimum" << ": " << (complete?"yes":"unknown") << "\n";
				cout << c << setw(w) << "Optimization" << ": ";
				out_->printOptimizeValues(*en.minimize());
				cout << "\n";
			}
		}
		if (facade_->step() > 0)
		{
			cout << c << setw(w) << "Total Steps" <<": " << facade_->step()+1 << endl;
		}
		cout << c << setw(w) << "Time" << ": " << fixed << setprecision(3) << timer_[0].elapsed() << endl;
		cout << c << setw(w) << "  Prepare" << ": " << fixed << setprecision(3) << timer_[ClaspFacade::state_read].elapsed() << endl;
		cout << c << setw(w) << "  Prepro." << ": " << fixed << setprecision(3) << timer_[ClaspFacade::state_preprocess].elapsed() << endl;
		cout << c << setw(w) << "  Solving" << ": " << fixed << setprecision(3) << timer_[ClaspFacade::state_solve].elapsed() << endl;
	}
	if (cmdOpts_.basic.stats) { out_->printStats(s.stats, en); }
}

#undef STATUS

int main(int argc, char **argv) {
    return GCAspApp::instance().run(argc, argv);
}

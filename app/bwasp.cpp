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
#include <sstream>
#include <vector>
#include <map>
#include <unordered_map>
#include <utility>
#include <stdlib.h>
#include <stdio.h>
#include <boost/regex.hpp>
#include <initializer_list>
#include <tuple>
#include <clasp/program_rule.h>

//using namespace Clasp;

typedef std::string Value;
typedef unsigned int uint;

/*
namespace std {
    std::string to_string(uint v) {
        std::stringstream ss;
        ss << v;
        return ss.str();
    }
}

template <class... Types>
struct Predicate {
	std::string name;
	typedef std::tuple<Types...> Key;
	typedef Clasp::ProgramBuilder::Var Value;
	typedef std::unordered_map<key, Value> Index;
	Index index;

	Predicate(std::string n) : name(n) {}

	bool add(Value v, Types... args) {
		return std::get<2>( index.emplace(std::make_tuple(args), v) );
	}

	Value get(Types... args) {
		auto it = index.find(std::make_tuple(args));
		if (it != index.end()) {
			return std::get<2>(*it);
		} else {
			assert("Not value found");
			return 0;
		}
	}
};
*/
struct Literal {
	std::string predicate;
	typedef Value Arg;
	typedef std::vector<Arg> ArgVec;
	ArgVec args;
	Literal() {}
    Literal(std::string p, ArgVec& a) : predicate(p), args(a) { update(); }
    Literal(std::string p, std::initializer_list<Arg> as) : predicate(p), args(as) { update(); }
    Literal(Literal &&lit) : predicate(std::move(lit.predicate)), args(std::move(lit.args)),
    		repr(std::move(lit.repr)) {}
    Literal(const Literal &lit) : predicate(lit.predicate), args(lit.args), repr(lit.repr) {}

    std::string repr;
    std::string str() const {
    	return repr;
    }
    void update() {
    	std::stringstream ss;
    	ss << predicate;
    	if (args.size() > 0) {
    		ss << "(";
    		for (uint i = 0; i < args.size() - 1; i++) {
    			ss << args[i] << ",";
    		}
    		ss << args[args.size() - 1] << ")";
    	}
    	repr = ss.str();
    }
};

inline bool operator <(const Literal& lhs, const Literal& rhs) {
	//return lhs.str() < rhs.str();
    return std::tie( lhs.predicate, lhs.args ) < std::tie( rhs.predicate, rhs.args );
}

struct Atom {
	typedef unsigned int Id;
	Id id;
	unsigned int generation;
    Atom(Id i, unsigned int g) : id(i), generation(g) {}
};

struct AtomGenarator {
	unsigned int cur;
	unsigned int generation;
    AtomGenarator(): cur(3), generation(1) {}
	Atom next() {
        Atom r (cur++, generation);
        return r;
	}
	void proceed() { generation++; }
};

namespace std {

// {{{ definition of hash functions for Literal

template<>
struct hash<Literal> {
    size_t operator()(Literal const &lit) const {
    	return std::hash<std::string>()(lit.str());
    }
};

template<>
struct equal_to<Literal> {
	bool operator()(const Literal &lhs, const Literal &rhs) const
	{
	    return lhs.str() == rhs.str();
	}
};
}

typedef std::unordered_map<Literal, Atom> LitAtomMap;
//typedef std::map<Literal, Atom> LitAtomMap;

class BWAspApp : public GringoApp, public Clasp::ClaspFacade::Callback
{
private:
	class LuaImpl;
	typedef std::auto_ptr<LuaImpl> LuaImplPtr;
	LuaImplPtr luaImpl;

public:
	/** returns a singleton instance */
	static BWAspApp& instance();

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

struct BWInput: public Clasp::Input
{
	typedef std::auto_ptr<ClaspOutput> OutputPtr;
	typedef Clasp::MinimizeConstraint* MinConPtr;
    using ArgVec = Literal::ArgVec;

	BWInput(std::istream &s);
	Format format() const { return Clasp::Input::SMODELS; }
	MinConPtr getMinimize(Clasp::Solver& s, Clasp::ProgramBuilder* api, bool heu) { return api ? api->createMinimize(s, heu) : 0; }
	void getAssumptions(Clasp::LitVec& a){}
	bool read(Clasp::Solver& s, Clasp::ProgramBuilder* api, int);
    void addLit(const Literal &lit, Clasp::ProgramBuilder *api);
    unsigned int getAtom(Literal& lit);
    bool nextStep(Clasp::ProgramBuilder *api);
    inline LitAtomMap& currentAtoms() {
    	if (t % 2 == 0) {
    		return curAtoms;
    	} else return prevAtoms;
    }
    inline bool exist(Literal& lit) {
    	return prevAtoms.find(lit) != prevAtoms.end() || curAtoms.find(lit) != curAtoms.end();
    }
    /*
	OutputPtr              out;
	IncConfig              config;
	Clasp::Solver*         solver;
    */
    typedef unsigned int uint;
    typedef std::vector< ::Literal> LitVec;
    LitVec blocks, inits, goals;
    Literal prevLevLit;
    uint t = 0; // current level.
    LitAtomMap curAtoms, prevAtoms;
    AtomGenarator atomGenerator;
};

#ifdef WITH_LUA
#	include "clingo/lua_impl.h"
#else

class BWAspApp::LuaImpl
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

BWInput::BWInput(std::istream &ifs): t(0)
{
    boost::regex block("block\\((\\w+)\\)\\.");
    boost::regex init("init\\((\\w+)\\,\\s*(\\w+)\\)\\.");
    boost::regex goal("goal\\((\\w+)\\,\\s*(\\w+)\\)\\.");
    //typedef std::match_results<const char*> cmatch;
    char line[LINESIZE];
    boost::cmatch match;
    while (ifs.good()) {
        ifs.getline(line, LINESIZE - 1);
        if (!ifs.good()) break;
        if (boost::regex_match(line, match, block) && match.size() > 0) {
        	ArgVec args;
        	args.push_back(match[1].str());
            blocks.push_back(Literal("block", args));
        } else if (boost::regex_match(line, match, init) && match.size() > 1) {
        	ArgVec args;
        	args.push_back(match[1].str());
        	args.push_back(match[2].str());
            inits.push_back(Literal("init", args));
        } else if (boost::regex_match(line, match, goal) && match.size() > 1) {
        	ArgVec args;
        	args.push_back(match[1].str());
        	args.push_back(match[2].str());
        	goals.push_back( Literal("goal", args) );
        }
    }
    std::cout << "Num of blocks: " << blocks.size() << std::endl;
    std::cout << "Num of inits: " << inits.size() << std::endl;
}

void BWInput::addLit(const Literal& lit, Clasp::ProgramBuilder *api) {
	Atom atom = atomGenerator.next();
	LitAtomMap &adb = currentAtoms();
    if (adb.find(lit) == adb.end()) {
        adb.insert(std::make_pair(lit, atom));
        std::string s = lit.str();

        api->setAtomName(atom.id, s.c_str());
    }
}

inline unsigned int BWInput::getAtom(Literal& lit) {
    LitAtomMap::const_iterator it = prevAtoms.find(lit);
    LitAtomMap::const_iterator it2 = curAtoms.find(lit);
    if (it != prevAtoms.end()) {
    	return it->second.id;
    } else if (it2 != curAtoms.end()) {
    	return it2->second.id;
    } else {
    	assert(false);
    }
}

bool BWInput::read(Clasp::Solver& s, Clasp::ProgramBuilder* api, int)
{
    /* on(X, Y, 0) :- init(X, Y). */
    if (t == 0) {
        for (uint i = 0; i < inits.size(); i++) {
            std::string x, y;
            x = inits[i].args[0];
            y = inits[i].args[1];
            //sprintf(buf, "on(%d,%d,0)", x, y);
            Literal on ( "on", { x, y, "0"} );
            addLit(on, api);
            //Var on = i + offset;
            //api->setAtomName(on, buf);
            api->startRule();
            api->addHead( getAtom(on) );
            api->endRule();
        }
        for (uint i = 0; i < goals.size(); i++) {
            std::string x, y;
            x = goals[i].args[0];
            y = goals[i].args[1];
            Literal on ( "on", { x, y, "0"} );
            addLit(on, api);
        }
        api->setCompute(2, false);
    } else {
    	api->setCompute(getAtom(prevLevLit), false);
    }
    return nextStep(api);
}

bool BWInput::nextStep(Clasp::ProgramBuilder *api) {

	// Add level Literal.
	std::stringstream ss;
	ss << "##" << t;
	prevLevLit.predicate = ss.str();
	prevLevLit.update();
	addLit(prevLevLit, api);

	Clasp::Var levelVar = getAtom(prevLevLit);
	api->startRule(Clasp::CHOICERULE);
	api->addHead( levelVar );
	api->endRule();

	api->setLevelVar( levelVar );

    std::string tstr = std::to_string(t);
    for (uint i = 0; i < blocks.size(); i++) {
        std::string x = blocks[i].args[0];
        Literal moved ( "moved", { x, tstr });
        Literal covered ( "covered", { x, tstr });
        addLit(moved, api);
        addLit(covered, api);
        for (uint j = 0; j <= blocks.size(); j++) {
            if (j != i) {
                std::string y;
                if (j == blocks.size()) { y = "table"; }
                else y = blocks[j].args[0];
                Literal move ( "move",  {x, y, tstr} );
                Literal nmove ( "nmove", {x, y, tstr} );
                Literal on ( "on", { x, y, std::to_string(t+1) });
                addLit(move, api);
                addLit(nmove, api);
                addLit(on, api);
            }
        }
    }
    // start adding rules.
    for (uint i = 0; i < blocks.size(); i++) {
        std::string x = blocks[i].args[0];
        /* covered(X, T) :- on(A, X, T), block(X). */
        Literal covered ( "covered", { x, std::to_string(t) } );
        for (uint j = 0; j < blocks.size(); j++) {
            if (j == i) continue;
            Literal on ("on", { blocks[j].args[0], x, tstr });
            if (t > 0 || exist(on)) {
                api->startRule();
                api->addHead( getAtom(covered) );
                api->addToBody(getAtom(on), true);
                api->endRule();
            }
        }
        Literal moved ( "moved", { x, std::to_string(t) });
/*
 *
nmove(X, Y, T) :- move(X1, Y, T), block(X), location(Y), time(T), ( X1 != X | Y1 != Y).
 */
        for (uint j = 0; j <= blocks.size(); j++) {
            if (j == i) continue; // X != Y

            std::string y;
            if (j == blocks.size()) { y = "table"; }
            else { y = blocks[j].args[0]; }
            Literal nmove (
                "nmove",
                    {
                        x, y, std::to_string(t)
                    }
            );
            for (uint k = 0; k < blocks.size(); k++) {
                for (uint l = 0; l <= blocks.size(); l++) {
                    if (l == k) continue;
                    if (k == i && l == j) continue; // X1 == X & Y1 == Y
                    std::string y1;
                    if (l == blocks.size()) { y1 = "table"; }
                    else  { y1 = blocks[l].args[0]; }
                    Literal move (
                        "move",
                            {
                                blocks[k].args[0],
                                y1,
                                std::to_string(t)
                            }
                    );
                    api->startRule();
                    api->addHead( getAtom(nmove) );
                    api->addToBody( getAtom(move), true );
                    api->endRule();
                }
            }
    /*
       move(X, Y, T) :- block(X), location(Y), time(T), not covered(X, T), not covered(Y, T), not nmove(X, Y, T), X != Y, T < t.
    */
            Literal move (
                "move",
                    {
                        x, y, std::to_string(t)
                    }
            );
            Literal xcov ( "covered", { x, std::to_string(t) });
            Literal ycov ( "covered", { y, std::to_string(t) });
            api->startRule();
            api->addHead( getAtom(move) );
            api->addToBody( getAtom(xcov), false );
            if (y != "table") {
                api->addToBody( getAtom(ycov), false );
            }
            api->addToBody( getAtom(nmove), false );
            api->endRule();

            /* moved(X, T) :- move(X, Y, T). */
            api->startRule();
            api->addHead( getAtom(moved) );
            api->addToBody( getAtom(move), true );
            api->endRule();

            Literal on ( "on", { x, y, std::to_string(t+1) });
            /* on(X, Y, T+1) :- move(X, Y, T), time(T). */
            api->startRule();
            api->addHead( getAtom(on) );
            api->addToBody( getAtom(move), true);
            api->endRule();

            /* on(X, Y, T+1) :- on(X, Y, T), time(T), not moved(X, T). */
            Literal pre ( "on", {x, y, tstr} );
            if (t > 0 || exist(pre)) {
                api->startRule();
                api->addHead( getAtom(on) );
                api->addToBody( getAtom(pre), true);
                api->addToBody( getAtom(moved), false);
                api->endRule();
            }
        }
    }
    for (uint i = 0; i < goals.size(); i++) {
        std::string x,y;
        x = goals[i].args[0];
        y = goals[i].args[1];
        Literal on ( "on", { x, y, std::to_string(t+1) });
        api->startRule();
        api->addHead(2);
        api->addToBody( getAtom(on), false);
        api->addToBody(levelVar, true);
        api->endRule();
    }
    /*std::cout << "Step " << t << std::endl;
    api->writeProgram(std::cout);*/
    t++;
    atomGenerator.proceed();
    //clear unneeded atoms from map
    if (t % 2 == 1) {
    	prevAtoms.clear();
    } else {
    	curAtoms.clear();
    }
    return true;
}


/////////////////////////////////////////////////////////////////////////////////////////
// BWAspApp
/////////////////////////////////////////////////////////////////////////////////////////

BWAspApp &BWAspApp::instance()
{
	static BWAspApp app;
	return app;
}

void BWAspApp::printVersion() const
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

std::string BWAspApp::getVersion() const {
	std::string r(GRINGO_VERSION);
	r += " (clasp ";
	r += CLASP_VERSION;
	r += ")";
	return r;
}

void BWAspApp::handleSignal(int) {
	for(int i = 0; i != sizeof(timer_)/sizeof(Timer); ++i)
		timer_[i].stop();
	fprintf(stderr, "\n*** INTERRUPTED! ***\n");
	if(facade_ && facade_->state() != Clasp::ClaspFacade::state_not_started)
		printResult(BWAspApp::reason_interrupt);
	_exit(solver_.stats.solve.models != 0 ?  S_SATISFIABLE : S_UNKNOWN);
}

void BWAspApp::configureInOut(Streams& s)
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
		in_.reset(new BWInput(std::cin)); //FIXME: hard coded into stdin stream.
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

int BWAspApp::doRun()
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

void BWAspApp::state(Clasp::ClaspFacade::Event e, Clasp::ClaspFacade& f) {
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

void BWAspApp::event(Clasp::ClaspFacade::Event e, Clasp::ClaspFacade& f)
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

void BWAspApp::printResult(ReasonEnd end)
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
    return BWAspApp::instance().run(argc, argv);
}

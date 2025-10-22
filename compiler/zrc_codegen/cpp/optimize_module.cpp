#include "llvm/IR/Module.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/TargetParser/Host.h"
#include "llvm/Support/TargetSelect.h"

using namespace llvm;

extern "C" void zrc_codegen_optimize_module(Module *M, TargetMachine *TM, uint32_t opt_level)
{
    OptimizationLevel ol;
    switch (opt_level)
    {
    // these are defined in the inkwell ABI
    case 0:
        ol = OptimizationLevel::O0;
        break;
    case 1:
        ol = OptimizationLevel::O1;
        break;
    case 2:
        ol = OptimizationLevel::O2;
        break;
    case 3:
        ol = OptimizationLevel::O3;
        break;
    }

    // Create the analysis managers.
    // These must be declared in this order so that they are destroyed in the
    // correct order due to inter-analysis-manager references.
    LoopAnalysisManager LAM;
    FunctionAnalysisManager FAM;
    CGSCCAnalysisManager CGAM;
    ModuleAnalysisManager MAM;

    // Create the new pass manager builder.
    // Take a look at the PassBuilder constructor parameters for more
    // customization, e.g. specifying a TargetMachine or various debugging
    // options.
    PassBuilder PB(TM);

    // Register all the basic analyses with the managers.
    PB.registerModuleAnalyses(MAM);
    PB.registerCGSCCAnalyses(CGAM);
    PB.registerFunctionAnalyses(FAM);
    PB.registerLoopAnalyses(LAM);
    PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

    // Create the pass manager.
    ModulePassManager MPM = PB.buildPerModuleDefaultPipeline(ol);

    // Optimize the IR!
    MPM.run(*M, MAM);
}

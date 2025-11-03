package de.z0rdak.yawp.platform.services.config;

import de.z0rdak.yawp.api.events.flag.FlagCheckRequest;
import de.z0rdak.yawp.api.events.flag.FlagCheckResult;

import java.util.Set;

public interface ILoggingConfigHelper {
    Set<String> getFlagsToLog();
    Set<String> getFlagCategories();
    Set<String> getResultValuesToLog();
    boolean logCheck(FlagCheckRequest check);
    FlagCheckResult logResult(FlagCheckResult result);
    boolean shouldLogFlagChecks();
    boolean shouldLogFlagCheckResults();
    boolean shouldLogEmptyResults();
}

package de.z0rdak.yawp.platform.config;

import de.z0rdak.yawp.api.events.flag.FlagCheckRequest;
import de.z0rdak.yawp.api.events.flag.FlagCheckResult;
import de.z0rdak.yawp.platform.event.ForgeFlagCheckRequest;
import de.z0rdak.yawp.platform.event.ForgeFlagCheckResult;
import de.z0rdak.yawp.config.server.LoggingConfig;
import de.z0rdak.yawp.platform.services.config.ILoggingConfigHelper;

import java.util.Set;

public class ForgeLoggingConfigHelper implements ILoggingConfigHelper {

    @Override
    public Set<String> getFlagsToLog() {
        return LoggingConfig.getFlagsToLog();
    }

    @Override
    public Set<String> getFlagCategories() {
        return LoggingConfig.getFlagTags();
    }

    @Override
    public Set<String> getResultValuesToLog() {
        return LoggingConfig.getResultValuesToLog();
    }

    @Override
    public boolean logCheck(FlagCheckRequest check) {
        ForgeFlagCheckRequest event = new ForgeFlagCheckRequest(check.getTarget(), check.getRegionFlag(), check.getDimension(), check.getPlayer(), check.getId());
        return LoggingConfig.logCheck(event);
    }

    @Override
    public FlagCheckResult logResult(FlagCheckResult result) {
        ForgeFlagCheckResult event = ForgeFlagCheckResult.asEvent(result);
        LoggingConfig.logResult(event);
        return ForgeFlagCheckResult.asNonEvent(event);
    }

    @Override
    public boolean shouldLogFlagChecks() {
        return LoggingConfig.shouldLogFlagChecks();
    }

    @Override
    public boolean shouldLogFlagCheckResults() {
        return LoggingConfig.shouldLogFlagCheckResults();
    }

    @Override
    public boolean shouldLogEmptyResults() {
        return LoggingConfig.shouldLogEmptyResults();
    }
}
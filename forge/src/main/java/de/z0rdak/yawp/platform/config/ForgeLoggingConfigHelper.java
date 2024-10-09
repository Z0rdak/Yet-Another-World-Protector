package de.z0rdak.yawp.platform.config;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.api.events.region.FlagCheckResult;
import de.z0rdak.yawp.api.events.region.ForgeFlagCheckEvent;
import de.z0rdak.yawp.api.events.region.ForgeFlagCheckResult;
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
        return LoggingConfig.getFlagCategories();
    }

    @Override
    public Set<String> getResultValuesToLog() {
        return LoggingConfig.getResultValuesToLog();
    }

    @Override
    public boolean logCheck(FlagCheckEvent check) {
        ForgeFlagCheckEvent event = new ForgeFlagCheckEvent(check.getTarget(), check.getRegionFlag(), check.getDimension(), check.getPlayer(), check.getId());
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
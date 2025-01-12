package de.z0rdak.yawp.platform.config;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.api.events.region.FlagCheckResult;
import de.z0rdak.yawp.api.events.region.NeoForgeFlagCheckEvent;
import de.z0rdak.yawp.api.events.region.NeoForgeFlagCheckResult;
import de.z0rdak.yawp.config.server.LoggingConfig;
import de.z0rdak.yawp.platform.services.config.ILoggingConfigHelper;

import java.util.Set;

public class NeoForgeLoggingConfigHelper implements ILoggingConfigHelper {

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
        NeoForgeFlagCheckEvent event = new NeoForgeFlagCheckEvent(check.getTarget(), check.getRegionFlag(), check.getDimension(), check.getPlayer(), check.getId());
        return LoggingConfig.logCheck(event);
    }

    @Override
    public FlagCheckResult logResult(FlagCheckResult result) {
        NeoForgeFlagCheckResult event = NeoForgeFlagCheckResult.asEvent(result);
        LoggingConfig.logResult(event);
        return NeoForgeFlagCheckResult.asNonEvent(event);
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
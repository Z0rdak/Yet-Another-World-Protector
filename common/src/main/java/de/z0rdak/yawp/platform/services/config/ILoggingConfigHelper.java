package de.z0rdak.yawp.platform.services.config;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.api.events.region.FlagCheckResult;
import de.z0rdak.yawp.commands.CommandSourceType;
import net.minecraft.commands.CommandSourceStack;

import java.util.Set;

public interface ILoggingConfigHelper {
    Set<String> getFlagsToLog();
    Set<String> getFlagCategories();
    Set<String> getResultValuesToLog();
    boolean logCheck(FlagCheckEvent check);
    FlagCheckResult logResult(FlagCheckResult result);
    boolean shouldLogFlagChecks();
    boolean shouldLogFlagCheckResults();
    boolean shouldLogEmptyResults();
}

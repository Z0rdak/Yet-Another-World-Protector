package de.z0rdak.yawp.commands.suggestions;

import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.suggestion.SuggestionProvider;
import com.mojang.brigadier.suggestion.Suggestions;
import com.mojang.brigadier.suggestion.SuggestionsBuilder;
import de.z0rdak.yawp.api.core.RegionManager;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.Identifier;
import net.minecraft.resources.ResourceKey;

import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

/**
 * Suggests all dimensions currently tracked by the RegionManager.
 *
 * <p>
 * Only dimensions that have region data registered are suggested.
 * The returned identifiers correspond to dimension region identifiers
 * and can be used by commands operating on tracked levels.
 * </p>
 *
 * <p>
 * Example suggestions:
 * {@code minecraft:overworld},
 * {@code minecraft:the_nether},
 * {@code minecraft:the_end}
 * </p>
 */
public class TrackedLevelSuggestionProvider implements SuggestionProvider<CommandSourceStack> {

    @Override
    public CompletableFuture<Suggestions> getSuggestions(CommandContext<CommandSourceStack> context, SuggestionsBuilder builder) {
        var tracked = RegionManager.get().getLevels();
        tracked.forEach(level -> builder.suggest(level.toString(), Component.literal("Level: " + level)));
        return builder.buildFuture();
    }
}

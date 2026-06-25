package de.z0rdak.yawp.commands.suggestions;

import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.suggestion.SuggestionProvider;
import com.mojang.brigadier.suggestion.Suggestions;
import com.mojang.brigadier.suggestion.SuggestionsBuilder;
import de.z0rdak.yawp.api.core.RegionManager;
import de.z0rdak.yawp.core.region.GlobalRegion;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.network.chat.Component;

import java.util.concurrent.CompletableFuture;

/**
 * Suggests all known region identifiers that can be referenced by commands.
 * <p>
 * This suggestion provider is intended to be used with the /yawp region command where it suggests the valid regions.
 * </p>
 *
 * <p>The provider returns region identifiers in the following order:</p>
 * <ol>
 *     <li>Local regions in the command source's current dimension.</li>
 *     <li>The current dimension region itself.</li>
 *     <li>Local regions from all other tracked dimensions.</li>
 *     <li>All other dimension regions.</li>
 *     <li>The global region.</li>
 * </ol>
 *
 * <p>
 * Regions from the current dimension are intentionally suggested first,
 * as they are the most likely targets for region management commands.
 * </p>
 *
 * <p>
 * Suggested identifiers are fully qualified region identifiers, for example:
 * {@code minecraft:overworld},
 * {@code minecraft:overworld/spawn}, or
 * {@code yawp:global}.
 * </p>
 */
public class RegionSuggestionProvider implements SuggestionProvider<CommandSourceStack> {

    @Override
    public CompletableFuture<Suggestions> getSuggestions(CommandContext<CommandSourceStack> ctx, SuggestionsBuilder builder) {
        var level = ctx.getSource().getLevel();
        // add local regions from the level the source is executing the command in, if the level is tracked
        var maybeLrd = RegionManager.get().getLevelRegionData(level.dimension());
        maybeLrd.ifPresent(lrd -> {
            lrd.getLocalIds().forEach(builder::suggest); // e.g. minecraft:overworld/spawn, ...
            builder.suggest(lrd.getId().toString()); // e.g. minecraft:overworld
        });
        // add locals for all other dimensions - except the one above
        var levels = RegionManager.get().getLevelRegionData();
        levels.stream()
                // filter the dimension already added above
                .filter(r -> !r.getId().equals(level.dimension().identifier()))
                .forEach(lrd -> {
                    lrd.getLocalIds().forEach(builder::suggest); // e.g. <mod>:<level>/<region-name>, ...
                    builder.suggest(lrd.getId().toString()); // e.g. <mod>:<level>
                });
        // GLOBAL
        builder.suggest(GlobalRegion.GLOBAL.toString(), Component.literal("YAWPs global region which spans all levels."));
        return builder.buildFuture();
    }
}

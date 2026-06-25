package de.z0rdak.yawp.commands.suggestions;

import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.suggestion.SuggestionProvider;
import com.mojang.brigadier.suggestion.Suggestions;
import com.mojang.brigadier.suggestion.SuggestionsBuilder;
import de.z0rdak.yawp.api.core.RegionManager;
import net.minecraft.commands.CommandSourceStack;

import java.util.concurrent.CompletableFuture;

/**
 * Suggests local region identifiers from the command source's current
 * dimension only.
 *
 * <p>
 * Dimension regions and the global region are intentionally excluded.
 * This provider is intended for commands that operate exclusively on
 * local (markable) regions.
 * </p>
 *
 * <p>
 * Example suggestions:
 * {@code minecraft:overworld/spawn},
 * {@code minecraft:overworld/market}
 * </p>
 */
public class LocalRegionSuggestionProvider implements SuggestionProvider<CommandSourceStack> {

    @Override
    public CompletableFuture<Suggestions> getSuggestions(CommandContext<CommandSourceStack> ctx, SuggestionsBuilder builder) {
        RegionManager.get()
                .getLevelRegionData(ctx.getSource().getLevel().dimension())
                .ifPresent(lrd -> lrd.getLocalIds().forEach(builder::suggest));
        return builder.buildFuture();
    }
}
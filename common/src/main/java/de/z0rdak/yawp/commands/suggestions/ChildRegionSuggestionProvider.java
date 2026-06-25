package de.z0rdak.yawp.commands.suggestions;

import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.suggestion.SuggestionProvider;
import com.mojang.brigadier.suggestion.Suggestions;
import com.mojang.brigadier.suggestion.SuggestionsBuilder;
import de.z0rdak.yawp.api.core.RegionManager;
import de.z0rdak.yawp.commands.arguments.ArgumentUtil;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.data.region.LevelRegionData;
import net.minecraft.commands.CommandSourceStack;

import java.util.concurrent.CompletableFuture;

/**

 */
public class ChildRegionSuggestionProvider implements SuggestionProvider<CommandSourceStack> {

    @Override
    public CompletableFuture<Suggestions> getSuggestions(CommandContext<CommandSourceStack> context, SuggestionsBuilder builder) {
        IMarkableRegion parent = ArgumentUtil.getLocalRegionArgument(context);
        parent.getChildren().values()
                .stream()
                .map(IMarkableRegion.class::cast)
                .forEach(child -> builder.suggest(child.getName()));
        return builder.buildFuture();
    }
}
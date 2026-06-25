package de.z0rdak.yawp.commands.suggestions;

import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import com.mojang.brigadier.suggestion.SuggestionProvider;
import com.mojang.brigadier.suggestion.Suggestions;
import com.mojang.brigadier.suggestion.SuggestionsBuilder;
import de.z0rdak.yawp.api.FlagRegister;
import de.z0rdak.yawp.commands.arguments.ArgumentUtil;
import de.z0rdak.yawp.core.flag.IFlag;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.SharedSuggestionProvider;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;

import java.util.List;
import java.util.concurrent.CompletableFuture;

import static de.z0rdak.yawp.api.MessageSender.sendCmdFeedback;
import static de.z0rdak.yawp.util.ChatLinkBuilder.buildRegionInfoLink;

public class MissingFlagsSuggestionProvider implements SuggestionProvider<CommandSourceStack> {
    @Override
    public CompletableFuture<Suggestions> getSuggestions(CommandContext<CommandSourceStack> ctx, SuggestionsBuilder builder) throws CommandSyntaxException {
        var region = ArgumentUtil.getRegionArgument(ctx);
        List<String> flagsInRegion = region.getFlags().flags().stream()
                .map(IFlag::getName)
                .toList();
        List<String> flags = FlagRegister.getFlagNames();
        flags.removeAll(flagsInRegion);
        if (flags.isEmpty()){
            if (ctx.getSource().getPlayer() instanceof ServerPlayer player){
                sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.region.flag.all-flags", "Region %s already contains all flags!", buildRegionInfoLink(region)));
            }
            return Suggestions.empty();
        }
        return SharedSuggestionProvider.suggest(flags, builder);
    }
}
